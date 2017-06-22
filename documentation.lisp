#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

;; drain.lisp
(docs:define-docs
  (type drain
    "Superclass for all drain segments in Harmony.

A drain is a segment that only consumes audio and
sends it off somewhere outside of the pipeline for
further processing.

A class that implements this protocol must implement
the INITIALIZE-CHANNEL method, and must set the
DECODER slot to a valid function of two arguments,
the first of which being a number of samples to
consume, the second being the class instance itself.

See CL-MIXED:DRAIN
See SEGMENT
See DECODER
See CHANNEL-FUNCTION
See REMIX-FACTOR"))

;; fadable.lisp
(docs:define-docs
  (type fadable
    "Mixin class for all segments that support volume fading.

Any implementing class must implement a method for
VOLUME and (SETF VOLUME). Furthermore, the class
must call PERFORM-FADING at the end of each of its
mix iterations.

In order for the fading effect to be nice, the
number of samples processed in each iteration must
be relatively small.

See SEGMENT
See START-VOLUME
See TARGET-VOLUME
See FADE-COUNT
See FADE-END
See EASING-FUNCTION
See FADE
See PERFORM-FADING")
  
  (function start-volume
    "Accessor to the starting volume of the fading segment.

See FADABLE")
  
  (function target-volume
    "Accessor to the ending/target volume of the fading segment.

See FADABLE")
  
  (function fade-count
    "Accessor to the current sample count in the fading process.

The fade duration is counted in number of samples
from zero. If this is the same as FADE-END, no
fading is performed. Otherwise this is increased
by the number of samples processed every time
PERFORM-FADING is called.

See FADE-END
See TARGET-VOLUME
See START-VOLUME
See FADABLE")
  
  (function fade-end
    "Accessor to the sample count after which the target-volume is reached.

See FADE-COUNT
See TARGET-VOLUME
See START-VOLUME
See FADABLE")
  
  (function easing-function
    "Accessor to the function used to ease the volume.

This function should take one float argument in
the range [0,1] and should return another float
value close to the range [0,1]. It must not
necessarily match the range exactly.

See FADABLE")
  
  (function fade
    "Cause the segment to start fading as soon as possible.

TIME should be a float, specifying the duration
of the fade in seconds. If :FROM is not specified,
it is taken to be current volume of the segment.
BY should be a function of one float argument
and one return value.

See EASING-FUNCTION
See FADABLE")
  
  (function perform-fading
    "Perform the computations necessary to adjust the volume to fade.

You should call this function at the end of every
iteration of your primary mixing of the segment.

See FADABLE")
  
  (function ease-linear
    "Linear easing.

X -> X")
  
  (function ease-cubic-in
    "Cubic-in easing.

X -> X³")
  
  (function ease-cubic-out
    "Cubic-out easing.

X -> 1+(x-1)³")
  
  (function ease-cubic-in-out
    "Cubic-in-out easing.

X -> (2*x)³/2       | If x <  0.5
     1+(2*(x-1))³/2 | otherwise"))

;; mixers.lisp
(docs:define-docs
  (type mixer
    "Superclass for all mixer segments.

Mixers are segments that accumulate multiple inputs.
They all allow you to add or remove sources to mix
from dynamically at runtime without having to re-
compile the entire pipeline. Any source dynamically
attached to a mixer should /not/ be part of the
pipeline, as otherwise it is processed multiple times.

See SEGMENT
See BUFFERS
See ADD
See WITHDRAW
See SOURCCES")
  
  (function buffers
    "Accessor to the sequence of buffers.

The buffers are internal to this object and should
not be touched unless you really, really know what
you're doing.

See MIXER
See SERVER")
  
  (function sources
    "Returns a list of source segments attached to the mixer.

See MIXER.")
  
  (type linear-mixer
    "This mixer simply linearly adds all its sources together.

Note that unless you control the volume of your
sources carefully, this can easily result in the
clipping of the audio samples, and thus in distortion.
No dynamic range compression is applied whatsoever.

By default the mixer is initialised to input and
output two channels. Should you require more or less,
you must specify it via its initarg.

Each source connected to the mixer /must/ have as
many outputs as this mixer has channels.

See CL-MIXED:LINEAR-MIXER
See MIXER")
  
  (type space-mixer
    "This mixer mixes sources as if they were located in 3D space.

Note that unless you control the volume of your
sources carefully, this can easily result in the
clipping of the audio samples, and thus in distortion.
No dynamic range compression is applied whatsoever.

Each source only has to have a single output buffer,
which is then interpreted as a point audio source. If
you require different shapes of sources, you can get
that behaviour by modifying the location of the source
relative to the listener in such a way that the
closest point of the source's shape is the actual point
the source has. This won't account for overlapping and
such finer details of sound, but should be good enough
for most cases.

Note that unlike in CL-MIXED, where each source is
independent of the mixer, and you thus have to use the
INPUT-LOCATION/VELOCITY functions to adjust their
parameters, in Harmony the sources are tied to a mixer
and you can thus use the same LOCATION and VELOCITY
functions for sources as you would for the mixer. In
the case of the mixer, the location and velocity are
to be interpreted as those of the listener.

See CL-MIXED:SPACE
See MIXER
See LOCATION
See VELOCITY
See DIRECTION
See UP
See SOUNDSPEED
See DOPPLER-FACTOR
See MIN-DISTANCE
See MAX-DISTANCE
See ROLLOFF
See ATTENUATION"))

;; pipeline.lisp
(docs:define-docs
  (type out-port
    "A port to represent an output location on a segment.

See FLOW:OUT-PORT
See FLOW:N-PORT")
  
  (type in-port
    "A port to represent an input location on a segment.

See FLOW:IN-PORT
See FLOW:1-PORT")
  
  (type in-ports
    "A port to represent a segment's inputs for a segment that supports arbitrary input locations.

See FLOW:IN-PORT
See FLOW:N-PORT")
  
  (type node
    "A node to represent a segment in the pipeline.

You should construct this through MAKE-NODE.

See FLOW:DYNAMIC-NODE
See MAKE-NODE
See COMPLETE-SEGMENT
See NTH-OUT
See NTH-IN")
  
  (function make-node
    "Construct a new node instance for the given segment.

The segment must support the INFO operation and report
accurately on its number of inputs and outputs.
Otherwise, a proper node cannot be constructed.

See NODE
See SEGMENT")
  
  (function complete-segment
    "Complete the segment in the node and return it.

Completing it means assigning the buffers to the
inputs and outputs of the segment as appropriate.
The node's ports must have a BUFFER attribute for
this purpose.

See NODE
See SEGMENT
See CL-MIXED:INPUT
See CL-MIXED:OUTPUT
See FLOW:ATTRIBUTE")
  
  (function nth-out
    "Return the nth output port of the node.

If no such output port exists, an error is signalled.

See NODE")
  
  (function nth-in
    "Return the nth input port of the node.

If no such input port exists, an error is signalled.
Note that IN-PORTS act as if they cover an indefinite
number of locations.

See NODE")
  
  (type pipeline
    "This class holds a collection of nodes during the assembly of an audio pipeline.

It is primarily used to handle the logic of connecting
segments together and finally compiling it all down
to a fully assembled audio pipeline ready for use in
the server.

Among other things it takes care of buffer allocation
and proper buffer assignment.

See CONNECT
See DISCONNECT
See SEVER
See COMPILE-PIPELINE")
  
  (function nodes
    "Accessor to the hash table mapping segments to their corresponding nodes.

See PIPELINE")
  
  (function ensure-node
    "Either creates or returns the node corresponding to the given segment.

See PIPELINE
See NODES
See MAKE-NODE")
  
  (function connect
    "Connects the output location to the input location of the given segments.

The system cannot feasibly figure out how you intend
locations to be connected together, which is why you
must specify them yourself.

See PIPELINE
See DISCONNECT
See SEVER")
  
  (function disconnect
    "Disconnects the output location from the input location of the given segments.

The system cannot feasibly figure out how you intend
locations to be disconnected, which is why you must
specify them yourself.

See PIPELINE
See CONNECT
See SEVER")
  
  (function sever
    "Severs all connections to and from the given segment.

See CONNECT
See DISCONNECT
See PIPELINE")
  
  (function allocate-buffers
    "Handle the allocation of the necessary buffers for the nodes.

The number of buffers necessary is determined by
looking at the BUFFER attribute of each port and
maximising that.

If OLD-BUFFERS is given, only additional buffers
as necessary are allocated and the buffers in the
old sequence are re-used.

After all buffers have been allocated, the BUFFER
attribute on each port in the nodes list is
replaced by the actual buffer instance.

See FLOW:ATTRIBUTE
See CL-MIXED:MAKE-BUFFER")
  
  (function compile-pipeline
    "Compile the pipeline into the given server.

This performs a buffer allocation on each of the
inputs and outputs of the segments connected in the
pipeline, flattens the graph topologically to
figure out the proper order to mix the segments in,
and finally constructs a mixer object to configure
the server with.

Once everything has been properly built, the server
is updated with the new device, buffers, and mixer.
The device is taken to be the last segment in the
topological sort.

After everything has been updated, the old mixer
and unneeded buffers are freed. Segments are not
freed, and will instead be garbage-collected
automatically if no longer needed.

This also updates the SEGMENT-MAP in the server, by
clearing it first, and then updating it with all
the segments contained in the pipeline.

See PIPELINE
See SERVER
See ALLOCATE-BUFFERS
See FLOW:ALLOCATE-PORTS
See COMPLETE-SEGMENT
See SEGMENT-MAP"))

;; segment.lisp
(docs:define-docs
  (type segment
    "Base class for all Harmony segments that can appear in a pipeline.

A segment must carry a reference to the server
that it is running on. This reference can be
supplied through the :SERVER initarg.

Note that even segments that are not derived
from this class, but rather the underlying class
CL-MIXED:SEGMENT may be used in a pipeline.

See CL-MIXED:SEGMENT")
  
  (function server
    "Accessor to the segment's server.

See SEGMENT")
  
  (function name
    "Accessor to the name of the segment.

The name should be a symbol, and can be used to
retrieve the segment from the server after the
pipeline has been compiled for it.

See SEGMENT"))

;; server.lisp
(docs:define-docs
  (variable *in-processing-thread*
    "If this is T, then the current context is within the server's processing thread.

See PROCESS
See CALL-IN-SERVER-THREAD")
  
  (type server
    "This class represents a sound server.

The server is responsible for keeping a background
thread that constantly processes and outputs sound.

The actual construction of a sound pipeline should
be done through the PIPELINE class, which can then
compile its information down to a format suitable
for the server. 

Upon construction the server must know the general
buffer size and the sample rate used throughout the
pipeline. These properties may not change unless
you completely recreate every segment and recompile
the pipeline.

The buffer size defaults to 441 and the sample rate
to 44100. Note that having too high a buffer size
will result in degraded output quality for some
operations like fading. Note that having a sample
rate that differs from your inputs or the output
device will likely result in bad audio quality and
distortions, as the audio data needs to be
resampled.

See SEGMENT-MAP
See BUFFERSIZE
See SAMPLESIZE
See DEVICE
See PIPELINE-MIXER
See BUFFERS
See THREAD
See COMPILE-PIPELINE
See EVALUATION-QUEUE
See EVALUATION-LOCK
See SEGMENT
See SEGMENTS
See START
See STOP
See PROCESS
See CALL-IN-SERVER-THREAD
See PAUSED-P
See PAUSE
See RESUME")
  
  (function segment-map
    "Accessor to the hash table associating names to segments.

This map is usually populated by COMPILE-PIPELINE.

See NAME
See SERVER
See SEGMENT")
  
  (function buffersize
    "Returns the number of samples in a buffer on the server.

See SERVER")
  
  (function samplerate
    "Returns the sample rate (in Hz) that the segments on the server operate by.

See SERVER")
  
  (function device
    "Accessor to the device segment of the server.

The device segment is like any other segment in
the pipeline, with the exception that the server
uses its PAUSED-P state to determine whether it
should suspend mixing or not. Usually COMPILE-
PIPELINE will ensure that the device is the last
segment in the sorted segments list, and thus
the segment responsible for outputting the audio
data.

See SERVER")
  
  (function pipeline-mixer
    "Accessor to the 'mixer' of the server.

This object handles the invocation of the segments
in their proper order. It is usually created and
assigned by COMPILE-PIPELINE.

See CL-MIXED:MIXER")
  
  (function thread
    "Accessor to the background processing thread of the server.

If this is NIL, you may assume that the server
is currently not processing any data.

See START
See STOP
See PROCESS")
  
  (function evaluation-queue
    "Accessor to the list of functions the server should evaluate.

This list is used to cause functions to be run
inside the server thread, forcing a
synchronisation of the state. This in turn allows
you to evaluate things that might otherwise cause
race conditions or other kinds of problems that
might wreck the system.

See SERVER
See CALL-IN-SERVER-THREAD")
  
  (function evaluation-lock
    "Accessor to the lock used to secure the evaluation queue.

See SERVER
See EVALUATION-QUEUE")
  
  (function segment
    "Accessor to the segment of the given name on the server.

This is useful to retrieve segments at a later
point in time after the pipeline has already been
assembled.

See SEGMENT-MAP
See SERVER")
  
  (function start
    "Start up the server's background processing thread.

This in turn causes audio processing to \"actually\"
happen. If the server is not ready to process audio,
an error is signalled. If the server has already been
started, an error is signalled.

See THREAD
See SERVER
See STOP")
  
  (function stop
    "Stops the server or source.

When the server is stopped, its background processing
thread is shut down. If no thread is currently
running, nothing happens.

When a source is stopped, its ENDED-P property is
set to T.

See THREAD
See SERVER
See START
See ENDED-P")
  
  (function process
    "This function is called by the background processing thread of the server.

It is responsible for starting, ending, and mixing
the segments in the pipeline. It is also
responsible for checking the server's evaluation-
queue, and running any function that appears on it.

Within PROCESS, *IN-PROCESSING-THREAD* is bound to
T.

If the device of the server is PAUSED-P, then
BT:THREAD-YIELD is called.

See SERVER")
  
  (function call-in-server-thread
    "Causes the given function to be evaluated in the server's processing thread.

If SYNCHRONIZE is non-NIL, then this function does
not return until the given function has been
evaluated. If additionally VALUES is non-NIL, the
values as produced by the given function are
returned after evaluation.

If TIMEOUT is given, it should be the number of
seconds after which this function gives up waiting
for the given function to be evaluated or to finish
evaluating.

Note that if *IN-PROCESSING-THREAD* is non-NIL, or
if the server has no thread, the function is
evaluated directly and immediately. Otherwise it
is placed onto the server's evaluation-queue.

Any call to this will acquire the evaluation-lock
of the server at least once.

See WITH-BODY-IN-SERVER-THREAD
See *IN-PROCESSING-THREAD*
See EVALUATION-QUEUE
See EVALUATION-LOCK")
  
  (function with-body-in-server-thread
    "Shorthand macro to evaluate the body in the server's processing thread.

See CALL-IN-SERVER-THREAD"))

;; source.lisp
(docs:define-docs
  (variable *filetype-source-map*
    "This map stores the associations of file type extensions to source class names.

See SOURCE-TYPE")
  
  (type source
    "Superclass for all of Harmony's source segments.

A source segment does not input any data and
instead directly provides it, either by generating
it on the fly, or by reading it from a file or
some kind of stream.

A class that implements this protocol must implement
the INITIALIZE-CHANNEL method, and must set the
DECODER slot to a valid function of two arguments,
the first of which being a number of samples to
consume, the second being the class instance itself.
Furthermore it must implement the SEEK-TO-SAMPLE
method to provide seeking. If it cannot seek, even
in the most imprecise way, it must implement that
method to signal an error.

See CL-MIXED:SOURCE
See INITIALIZE-CHANNEL
See FADABLE
See LOOPING-P
See PAUSED-P
See ENDED-P
See DECODER
See SAMPLE-POSITION
See REMIX-FACTOR
See CHANNEL-FUNCTION
See MIXER
See PAUSE
See RESUME
See STOP
See SEEK
See SEEK-TO-SAMPLE
See LOCATION
See VELOCITY")
  
  (function looping-p
    "Accessor to whether the source should loop after finishing.

See SOURCE")
  
  (function paused-p
    "Accessor to whether the source is paused or not.

See SOURCE
See PAUSE
See RESUME")
  
  (function ended-p
    "Accessor to whether the source has reached its end or not.

Once a source has been marked as ended, it is
automatically paused as soon as possible, and then
removed from its mixer if the mixer is known to the
source.

And ended source can be started anew by using
the RESUME function.

See STOP
See RESUME")
  
  (function decoder
    "Accessor to the decoder function of the source or drain.

This function is responsible for processing the
audio samples between the external device it is
interacting with and the internal audio buffers.
For sources this means filling the raw audio
buffer of the channel, and for drains this means
processing the raw audio buffer of the channel.

This function is automatically called during the
mixing of a source or drain as long as it is not
paused and hasn't ended.

See SOURCE
See DRAIN")
  
  (function sample-position
    "Returns the sample counter for the source.

The sample counter is kept to keep track of the
seeking position. It is not necessarily entirely
accurate.

The samples are counted in the source's channel
samplerate, rather than the server's samplerate.
It is updated during mixing.

See SOURCE")
  
  (function remix-factor
    "Returns the conversion factor between the channel's and the server's samplerate.

See SOURCE
See DRAIN")
  
  (function channel-function
    "Accessor to the original C-function used to process the channel in a source/drain.

You should probably not have to touch this.

See SOURCE
See DRAIN")
  
  (function mixer
    "Accessor to the mixer that this source was added to.

See SOURCE")
  
  (function initialize-channel
    "This function creates the appropriate channel instance for use with the source/drain.

See CL-MIXED:CHANNEL
See SOURCE
See DRAIN")
  
  (function pause
    "Pause the source or drain.

See SOURCE
See DRAIN
See SERVER")
  
  (function resume
    "Resume the source or drain from a pause.

If the source was ENDED-P, it is seeked to
position 0 before resuming.

See SOURCE
See DRAIN
See SERVER")
  
  (function seek
    "Seek the source to the requested position.

MODE may be either :ABSOLUTE or :RELATIVE,
where relative denotes the requested position
to be relative to the current position.
BY may be either :SAMPLE or :SECOND, where
:SAMPLE denotes the specific requested sample
position in the source's stream, and :SECOND
requests the approximate playback time in
seconds.

See SOURCE
See SEEK-TO-SAMPLE")
  
  (function seek-to-sample
    "Seek to the specified sample.

This position is absolute. Every source
must implement a method for this function.

See SEEK
See SOURCE")

  (function source-type
    "Accessor to the file type extension to source type mapping.

The name should be a string designating the file
type, and the value a source class name.

See *FILETYPE-SOURCE-MAP*
See DEFINE-SOURCE-TYPE")

  (function define-source-type
    "Conveniently define a source type mapping.

See SOURCE-TYPE")

  (function play
    "Conveniently play back a file on the designated mixer of the server.

By default the source type to use is determined by
the file type extension in the given path. If no
corresponding source type is known, an error is
signalled.

The given mixer may either be a segment, or the name
of a registered segment on the server to add the
constructed source to.

See SEGMENT
See SOURCE-TYPE
See VOLUME
See FADE
See ADD"))
