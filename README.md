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

## Choosing the Playback Backend
Harmony includes support for a variety of playback backends: Out123, OpenAL, WASAPI, ALSA, and PulseAudio. It is recommended to use the native backends (WASAPI, ALSA) where possible as they do not have any C dependencies and should have reduced latency and complexity compared to the other backends (Out123, OpenAL).

You can choose a backend, and customise its options, with the `:output-spec` initarg on harmony-simple's `initialize`. For example, to use the PulseAudio backend:

    (asdf:load-system :harmony-pulse)
    (harmony-simple:initialize :output-spec '(harmony-pulse:pulse-drain))

A number of the backends include extra options like `:program-name` that allows you to change the appearance of your program in the system's volume mixer.

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

## Extending Harmony's Segments
Segments are the lifeblood of Harmony. Through segments you get audio data into the pipeline, transform it as you need, and finally even output it to be heard. Being able to define custom and new segments is therefore the primary way in which to extend Harmony's functionality.

One of the easiest things to do is to define a new source. Let's do that with a sine wave generator. As a first step, we'll need to define a class.

    (defclass sine (harmony:source cl-mixed:virtual)
      ((sine-phase :initform 0 :accessor sine-phase)))

The class inherits from `source` to get useful functionality for fading, pausing, and seeking. We also inherit from `cl-mixed:virtual` as this segment won't have a segment defined in C to support it underneath. Instead, we can now extend all the functionality of the segment from the lisp side.

As part of the contract of subclassing `source`, we need to implement a method on `seek-to-sample`. Let's do that:
    
    (defmethod harmony:seek-to-sample ((source sine) position)
      (setf (sine-phase source) (mod position (harmony:samplerate (harmony:server source)))))

We'll be using the `sine-phase` to track the current phase of our sine wave, so in order to "seek" we'll need to adjust it here. Naturally you won't really be able to hear the effects of seeking on a sine wave generator, but for other sources proper seeking behaviour can be vital.

Finally we need to implement the actual sample generation, which for all `source`s happens in `process`:
    
    (defmethod harmony:process ((source sine) samples)
      (let* ((buffers (cl-mixed:outputs source))
             (phase (sine-phase source))
             (samplerate (harmony:samplerate (harmony:server source)))
             (factor (* 2 PI 440 (/ samplerate))))
        (loop for i from 0 below samples
              for sample = (sin (coerce (* factor phase) 'single-float))
              do (loop for buffer across buffers
                       do (setf (cffi:mem-aref (cl-mixed:data buffer) :float i) sample))
                 (incf phase))
        (setf (sine-phase source) (mod phase samplerate))))

First we extract the output buffer vector and the current phase, and calculate the constant factor for the wave. We then iterate over the number of samples we need to generate, and set the sample at the corresponding position in each buffer. Note that buffers contain C arrays and as such we need to use CFFI's `mem-aref` instead of Lisp's `aref`. Once we're done with the sample generation we update the phase in our instance.

And that's it. You can now test your sine generator with `(harmony-simple:play 'sine :music)`.

For a lot of sources and drains you will want to interact with other libraries and systems. They will usually have their own idea of how the sample data should be represented. Often times they'll want a single C array where the samples are interleaved, or might even need a different sample format than the floats Harmony uses internally. For this, libmixed offers a good amount of support by taking over the packing and unpacking of samples for you. 

In Harmony, you can make use of this with the `unpack-source` and `pack-drain` respectively. As an additional contract, they'll have you construct a `packed-audio` object that encodes the sample representation format, but will in turn handle the conversion for you, so that you just need to write to or read from a buffer using your foreign system. For an example on how this works, see the [mp3](sources/mp3.lisp) and [wav](sources/wav.lisp) sources, or the [alsa](drains/alsa.lisp) and [pulse](drains/pulse.lisp) drains.

Finally I think it would be worth it to look at an example of an effect segment. We'll be writing a very primitive version of an echo effect with a linear falloff.

As before, we'll first want to create a new class to encapsulate our segment with.

    (defclass echo (harmony:segment cl-mixed:virtual)
      ((buffers :initform NIL :accessor buffers)
       (offset :initform 0 :accessor offset)
       (delay :initarg :delay :initform 0.2 :accessor delay)
       (falloff :initarg :falloff :initform 0.8 :accessor falloff)))

We'll make this work by having internal buffers that keep some of the previous samples that come in and mixes them back into the output with a slight delay added. In order to know the proper size of the buffers, we'll need to know the sample rate ahead of time as well, hence the parameter.

Next we'll need to actually instantiate the buffers on start.
    
    (defmethod cl-mixed:start ((echo echo))
      (let ((buffers (make-array 2)))
        (setf (buffers echo) buffers)
        (dotimes (i (length buffers))
          (setf (aref buffers i) (make-array (ceiling (* (delay echo)
                                                         (harmony:samplerate
                                                          (harmony:server echo))))
                                             :element-type 'single-float
                                             :initial-element 0.0s0)))))

Since we don't need to share these buffers with any C parts, we can just use a CL array for our convenience.

In order for Harmony to be able to assemble the pipeline from the segments properly, it needs to know some information about the segment. We can provide this information with a method on `cl-mixed:info`:

    (defmethod cl-mixed:info ((echo echo))
      (list :min-inputs 2 :max-inputs 2 :outputs 2))

There are a few other info fields that are specified by libmixed, but this should be enough for our purposes.

Finally, the actual mixing step. The `virtual` class provides methods for all of the actions that can be performed on a segment. It already handles the connecting of input and output buffers for us, so we don't need to care about that. All we need to do is handle the mixing logic.
    
    (defmethod cl-mixed:mix (samples (echo echo))
      (let ((offset (offset echo))
            (falloff (falloff echo)))
        (loop for out-buffer across (cl-mixed:outputs echo)
              for in-buffer across (cl-mixed:inputs echo)
              for out = (cl-mixed:data out-buffer)
              for in = (cl-mixed:data in-buffer)
              for buf across (buffers echo)
              for off = offset
              do (loop for i from 0 below samples
                       for sample = (cffi:mem-aref in :float i)
                       for echo = (aref buf off)
                       do (setf (cffi:mem-aref out :float i) (+ sample echo))
                          (setf (aref buf off) (* (+ sample echo) falloff))
                          (setf off (mod (1+ off) (length buf)))))
        (setf (offset echo) (mod (+ offset samples) (length (aref (buffers echo) 0))))))

This iterates over all channels (in->buf->out), and for each channel does the following: for each sample to compute, output the input sample added to the sample looked up in the echo buffer. Then store the same multiplied by the falloff into the echo buffer. Finally advance the offset into the echo buffer. At the end of the function we have to make sure to store the current offset in the segment for the next time mix is called.

In order to test this we'll create our own pipeline real quick:

    (let ((pipeline (make-instance 'harmony:pipeline))
          (output (harmony-simple:make-segment 'harmony-out123:out123-drain))
          (echo (harmony-simple:make-segment 'echo :name :echo))
          (source (harmony-simple:make-segment 'harmony-mp3:mp3-source :file #p"some.mp3")))
      (harmony:connect pipeline echo 0 output 0)
      (harmony:connect pipeline echo 1 output 1)
      (harmony:connect pipeline source 0 echo 0)
      (harmony:connect pipeline source 1 echo 1)
      (harmony:compile-pipeline pipeline harmony-simple:*server*))

Note that this will perform all the pipeline computation locally, and then switch out the pipeline in the server. If you had something else going on in the previous pipeline, then that will be replaced. You should now hear the song being played back with an echo effect.

For more information on the details of segments and the underlying mechanisms of the mixing, see the documentation of the [cl-mixed](https://shirakumo.github.io/cl-mixed) library.

## Also See

* [cl-mixed](https://shirakumo.github.io/cl-mixed) For the mixing and sound processing library.
* [flow](https://shinmera.github.io/flow) For the pipeline construction and graph handling.
