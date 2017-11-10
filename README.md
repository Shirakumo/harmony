## About Harmony
Harmony is a library that provides you with audio processing tools as well as an audio server to play back music, sfx, and so forth. It is most suited for use in a game engine, but may feasibly also used for more advanced things such as an audio processing tool.

## How To
A very basic sound system can be gained by loading the `harmony-simple` system and using its predefined audio pipeline and convenience functions.

    (ql:quickload :harmony-simple)

Before any audio can be played, the audio server needs to be initialised.

    (harmony-simple:initialize)

Once the server is running, we need some files to play. Harmony-simple sets up a pipeline that supports three distinct mixers: `:music`, `:sfx`, and `:voice`. The latter two mixers provide 3D audio, allowing you to place the sources in 3D space. Playing a file simply happens through the `play` function.

    (harmony-simple:play #p"music.mp3" :music :loop T)
    (harmony-simple:play #p"explosion.mp3" :sfx)

Returned by `play` is a `source` instance, which you can use to control the playback. You can `pause`, `resume`, and `stop` a source. When a source is stopped manually or naturally reaches its end it is removed from the mixer. You can `resume` and `add` it again if you want to re-use it. If you would like to smoothly fade a track, you can use the `:fade` initarg on `play`, or use the `fade` function after the fact. You can also directly control the volume with the `volume` accessor.

For sources on the 3D mixers, you can use the `location` and `velocity` accessors to modify their placement. The velocity will not cause the location to be updated automatically, and is instead used to calculate the intensity of the doppler effect. If you do not want to have a doppler effect, you can simply leave the velocity at its default. The mixer itself also has a few properties that you'll want to control in order to control the listener. Aside from the `location` and `velocity`, there's also the `direction`, `up`, `soundspeed`, `doppler-factor`, `min-distance`, `max-distance`, `rolloff`, and `attenuation` to control the properties of the 3D space.

If you need to pause the sound playback entirely, you can also use `pause` and `resume` without an argument.

## Constructing Your Own Pipeline
If you want a pipeline that is a bit more involved than what harmony-simple gives you, perhaps to add certain effects or other kinds of shenanigans, you can easily build that pipeline. Before you do, though, you should get briefly acquainted with [cl-mixed](https://shirakumo.github.io/cl-mixed), or at least with the segments it provides. Harmony uses libmixed underneath to do most of the audio processing and mixing, and in itself only provides a sound server, and a convenient way to handle segments and create a pipeline.

Here's an example of a pipeline consisting of an output, a LADSPA plugin, and a basic mixer. You'll need a server beforehand, which you can get by simply instantiating `harmony:server`.

    (let* ((pipeline (make-instance 'harmony:pipeline))
           (output (make-instance 'harmony-out123:out123-drain :server server))
           (master (make-instance 'harmony:basic-mixer :server server :name :master))
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

## Extending Harmony's Segment
Segments are the lifeblood of Harmony. Through segments you get audio data into the pipeline, transform it as you need, and finally even output it to be heard. Being able to define custom and new segments is therefore the primary way in which to extend Harmony's functionality.

One of the easiest things to do is to define a new source. Let's do that with a sine wave generator. As a first step, we'll need to define a class.

    (defclass sine (source cl-mixed:virtual)
      ((sine-phase :initform 0 :accessor sine-phase))

The class inherits from `source` to get useful functionality for fading, pausing, and seeking. We also inherit from `cl-mixed:virtual` as this segment won't have a segment defined in C to support it underneath. Instead, we can now extend all the functionality of the segment from the lisp side.

As part of the contract of subclassing `source`, we need to implement a method on `seek-to-sample`. Let's do that:
    
    (defmethod seek-to-sample ((source sine) position)
      (setf (sine-phase source) (mod position (samplerate (server source)))))

We'll be using the `sine-phase` to track the current phase of our sine wave, so in order to "seek" we'll need to adjust it here. Naturally you won't really be able to hear the effects of seeking on a sine wave generator, but for other sources proper seeking behaviour can be vital.

Finally we need to implement the actual sample generation, which for all `source`s happens in `process`:
    
    (defmethod process ((source sine) samples)
      (let ((buffers (outputs source))
            (phase (sine-phase source))
            (factor (* 2 PI 440 (/ (samplerate (server source))))))
        (loop for i from 0 below samples
              for sample = (sin (coerce (* factor phase) 'single-float))
              do (loop for buffer across buffers
                       do (setf (cffi:mem-aref (cl-mixed:data buffer) :float i) sample))
                 (incf phase))
        (setf (sine-phase source) (mod phase (samplerate (server source))))))

First we extract the output buffer vector and the current phase, and calculate the constant factor for the wave. We then iterate over the number of samples we need to generate, and set the sample at the corresponding position in each buffer. Note that buffers contain C arrays and as such we need to use CFFI's `mem-aref` instead of Lisp's `aref`. Once we're done with the sample generation we update the phase in our instance.

And that's it. You can now test your sine generator with `(harmony-simple:play 'sine :music)`.

STUFF STUFF STUFF

Naturally you'll want to read up on the underlying [cl-mixed](https://shirakumo.github.io/cl-mixed) library.

## Also See

* [cl-mixed](https://shirakumo.github.io/cl-mixed) For the mixing and sound processing library.
* [flow](https://shinmera.github.io/flow) For the pipeline construction and graph handling.
