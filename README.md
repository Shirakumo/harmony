## About Harmony
Harmony is a library that provides you with audio processing tools as well as an audio server to play back music, sfx, and so forth. It is most suited for use in a game engine, but may feasibly also used for more advanced things such as an audio processing tool.

## How To
A very basic sound system can be gained by loading the `harmony-simple` system and using its predefined audio pipeline and convenience functions.

    (ql:quickload :harmony-simple)

Before any audio can be played, the audio server needs to be started up.

    (harmony-simple:start)

Once the server is running, we need some files to play. Harmony-simple sets up a pipeline that supports three distinct mixers: `:music`, `:sfx`, and `:voice`. The latter two mixers provide 3D audio, allowing you to place the sources in 3D space. Playing a file simply happens through the `play` function.

    (harmony-simple:play #p"music.mp3" :music :loop T)
    (harmony-simple:play #p"explosion.mp3" :sfx)

Returned by `play` is a `source` instance, which you can use to control the playback. You can `pause`, `resume`, and `stop` a source. When a source is stopped manually or naturally reaches its end it is removed from the mixer. You can `resume` and `add` it again if you want to re-use it. If you would like to smoothly fade a track, you can use the `:fade` initarg on `play`, or use the `fade` function after the fact. You can also directly control the volume with the `volume` accessor.

For sources on the 3D mixers, you can use the `location` and `velocity` accessors to modify their placement. The velocity will not cause the location to be updated automatically, and is instead used to calculate the intensity of the doppler effect. If you do not want to have a doppler effect, you can simply leave the velocity at its default. The mixer itself also has a few properties that you'll want to control in order to control the listener. Aside from the `location` and `velocity`, there's also the `direction`, `up`, `soundspeed`, `doppler-factor`, `min-distance`, `max-distance`, `rolloff`, and `attenuation` to control the properties of the 3D space.

If you need to pause the sound playback entirely, you can also use `pause` and `resume` without an argument.

## Constructing Your Own Pipeline
If you want a pipeline that is a bit more involved than what harmony-simple gives you, perhaps to add certain effects or other kinds of shenanigans, you can easily build that pipeline. Before you do, though, you should get briefly acquainted with [cl-mixed](https://shirakumo.github.io/cl-mixed), or at least with the segments it provides. Harmony uses libmixed underneath to do most of the audio processing and mixing, and in itself only provides a sound server, and a convenient way to handle segments and create a pipeline.

Here's an example of a pipeline consisting of an output, a LADSPA plugin, and a linear mixer. You'll need a server beforehand, which you can get by simply instantiating `harmony:server`.

    (let* ((pipeline (make-instance 'harmony:pipeline))
           (output (make-instance 'harmony-out123:out123-drain :server server))
           (master (make-instance 'harmony:linear-mixer :server server :name :master))
           (ladspa (make-instance 'cl-mixed:ladspa :samplerate (harmony:samplerate server)
                                                   :file #p"ladspa-plugin.so")))
      (harmony:connect pipeline ladspa 0 output 0)
      (harmony:connect pipeline ladspa 1 output 1)
      (harmony:connect pipeline master 0 output 0)
      (harmony:connect pipeline master 1 output 1)
      (harmony:compile-pipeline pipeline server))

Once the pipeline has been compiled to the server, you can start by adding segments to the master mixer, similar to before. Most of the other commands remain the same as well, just using a different package.

    (harmony:play server #p"something.mp3" :master)

When you construct the pipeline, Harmony takes care of properly allocating the necessary internal buffers, connecting the segments as required, and determining the proper order in which to process them.

## Extending Harmony's Sources
If you would like to add support for an additional audio format, or some other kind of audio source, you'll want to look into creating your own source class. A relatively simple example of how to do so is shown in the [provided mp3 source](sources/mp3.lisp). Generally the procedure is as follows:

    (defclass my-source (source)
      ())
    
    (defmethod initialize-instance :after ((source my-source) &key)
      (setf (decoder source) #'decode))
    
    (defmethod initialize-channel ((source my-source))
      #| Read source and determine properties |#
      (cl-mixed:make-channel #| source file properties here |#))
    
    (defmethod seek-to-sample ((source my-source) position)
      #| Seek somehow |#)
    
    (defun decode (samples source)
      (let ((c-buffer (cl-mixed:data (cl-mixed:channel source))))
        #| Process the source file, write samples into c-buffer. |#
        #| Handle end-of-source / looping |#))

Naturally you'll want to read up on the underlying [cl-mixed](https://shirakumo.github.io/cl-mixed) library. The system should be able to handle conversion of sample types, buffer layouts, and even sample rates. All you need to do is properly initialise the channel object and write the raw data into the c-buffer.

When constructing the channel, you'll most likely want to try to match your source's sample rate, buffer size, and sample format with that of the server, if at all possible. The `server`, `samplerate`, and `buffersize` functions will be useful for that. The server's internal sample format is always single-floats.

You don't necessarily have to support seeking in your source, but it is highly recommended to take the effort to provide it, or in the very least provide a way to seek back to the beginning.

If your source does indeed read from a file, you'll probably also want to register the file type with the system, so that `play` automatically knows to use your source.

    (define-source-type "what" my-source)

## Extending Harmony's Drains
Drains function almost completely analogous to sources, with the exception that they don't fill the c-buffer, but rather read it out. Thus, drains would be useful to write to a file, or output them to a sound server on your operating system. The standard `harmony-out123:out123-drain` already provides the latter for you, so unless you need special support for that, there is no need to create additional ones.

## Also See

* [cl-mixed](https://shirakumo.github.io/cl-mixed) For the mixing and sound processing library.
* [flow](https://shinmera.github.io/flow) For the pipeline construction and graph handling.
