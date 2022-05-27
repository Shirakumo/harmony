#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony)

;; environment.lisp
(docs:define-docs
  (type environment
    "Encapsulates a playback environment.

An environment is used for horizontal mixing and contains a set of
track lists. The environment controls playback of the segments within
the track lists by ensuring that play heads are synchronised when
switching and allowing smooth transitioning between segments.

When initialising an environment, you may pass a :SETS argument, which
should be a list of the following structure:

  SETS ::= (SET*)
  SET  ::= (name SOURCE*)
  SOURCE ::= input | (input INITARGS*)

Where input and initargs are sutable for use with CREATE. The name
given names the state in which the environment must be for the
associated list of sources to be played back. The class used to create
is a MUSIC-SEGMENT.

The environment will continuously loop the current list of tracks
smoothly until it transitions to another state.

See STATE
See ACTIVE-P
See TRANSITION
See MUSIC-SEGMENT")
  
  (function state
    "Accesses the current state of the environment

Must be either the name of a source set available to the environment,
or NIL.

See ENVIRONMENT")
  
  (function active-p
    "Returns T if the environment is playing back a source set.

See ENVIRONMENT")
  
  (type music-segment
    "Subclass of VOICE that allows smooth transitions.

When initialised, takes the following initargs:

  :TRANSITION-POINTS
    A sequence of sample points at which a transition is considered to
    be favourable. If transition points are defined, the transition
    will be delayed until a transition point is hit. Otherwise, the
    transition will happen immediately.

  :TRANSITION-INTERVAL
  :TRANSITION-OFFSET
    Combined with :TRANSITION-OFFSET, defines the set of sample points
    where a transition is favourable. You may use this instead of
    :TRANSITION-POINTS if the points are always equidistant.
    Meaning, if (SAMPLE - TRANSITION-OFFSET % TRANSITION-INTERVAL) = 0
    a transition point is generated.

  :TRANSITION-FUN
    Instead of the above methods, you may also specify a manual
    function of two arguments, OLD and NEW sample indices, and returns
    a boolean indicating whether a transition may occur between OLD
    and NEW.

See TRANSITION
See ENVIRONMENT
See VOICE")
  
  (function transition
    "Transitions an environment or segment to another state.

May be used to transition an environment to a new STATE, transition
from one music-segment to another, or transition a music-segment's
volume to a new level.

In any case, takes :IN to denote the duration of the transition in
seconds and may take :SYNC to denote whether the playheads of two
music-segments should be synchronised to match before transitioning.

See ENVIRONMENT
See MUSIC-SEGMENT"))

;; segment.lisp
(docs:define-docs
  (type buffer
    "Superclass for MIXED:BIP-BUFFER.

Keeps track of connected segments.

See FROM
See FROM-LOCATION
See TO
See TO-LOCATION
See MIXED:BIP-BUFFER")
  
  (function from
    "Accesses the source segment of the buffer.

See BUFFER")
  
  (function from-location
    "Accesses the index on the source segment to which the buffer is attached.

See BUFFER")
  
  (function to
    "Accesses the drain segment of the buffer.

See BUFFER")
  
  (function to-location
    "Accesses the index on the drain segment to which the buffer is attached.

See BUFFER")
  
  (type segment
    "Superclass for MIXED:SEGMENT

Keeps track of an extra name and the chain on which the segment is
being played back.

See NAME
See CHAIN
See CONNECT
See DISCONNECT
See DOWNSTREAM
See UPSTREAM
See MIXED:SEGMENT")
  
  (function name
    "Returns the name of the segment.

See SEGMENT")
  
  (function chain
    "Accesses the chain of the segment.

See SEGMENT
See MIXED:CHAIN")
  
  (function connect
    "Connects one segment to another.

FROM/TO-LOC may also be T, in which case all available ports are
connected.

May allocate buffers on the current *SERVER* to facilitate the
connection.

See SEGMENT")
  
  (function disconnect
    "Disconnects a segment's port.

DIRECTION designates whether an output or an input port is
disconnected. After the port has been disconnected, the associated
buffer is freed in the *SERVER*.

See SEGMENT")
  
  (function downstream
    "Returns the segment connected downstream from this one on the specified port index.

See SEGMENT")
  
  (function upstream
    "Returns the segment connected upstream from this one on the specified port index.

See SEGMENT")
  
  (type source
    "Superclass for MIXED:SOURCE

Includes extra information to allow for looping and handling of input
termination.

When initialised, takes the following initargs:

   :REPEAT
     How many times to repeat the source's output. On each repeat, the
     source is seeked to the :REPEAT-START. Instead of an integer, may
     also be NIL for no repeats (default), or T for endless repeats.
     
   :REPEAT-START
     The second (!) timestamp to which the source should be seeked
     once its end is reached. Defaults to 0.0
     
   :ON-END
     A function to be called once the source reaches its end. An end
     here means that the source has completed all repeats that were
     scheduled and has now run out of samples to output back. By
     default the source is disconnected from its outputs.
     
   :ON-FRAME-CHANGE
     A function to be called when the source's frame position is
     changed. By default does nothing.

See MIXED:SOURCE
See SEGMENT
See REPEAT
See REPEAT-START
See ON-END
See ON-FRAME-CHANGE")
  
  (function repeat
    "Accesses the repeat count for the source.

May be an integer, NIL (0), or T (infinity).

See SOURCE")
  
  (function repeat-start
    "Accesses the start timestamp to which the source should be seeked on a repeat.

See SOURCE")
  
  (function on-end
    "Accesses the function to be called when the source reaches its end.

The function should take one argument, the SOURCE.

See SOURCE")

  (function on-frame-change
    "Accesses the function to be called when the source's frame position changed.

The function should take two arguments, the SOURCE, and the new frame position.

See SOURCE"))

;; server.lisp
(docs:define-docs
  (variable *server*
    "References the currently active SERVER instance, if any.

See SERVER")
  
  (type server
    "Class encapsulating a audio rendering server.

An audio server keeps track of buffers allocations, chains, segments,
synchronisation, coordination of samplerate, and manages rendering in
a background thread.

When initialized, sets *SERVER*.

See ALLOCATE-BUFFER
See ALLOCATE-UNPACKER
See FREE-BUFFER
See FREE-UNPACKER
See SEGMENT
See STARTED-P
See RUN-TASK
See RUN
See CALL-IN-MIXING-CONTEXT
See WITH-SERVER
See DOT-SERVER
See MIXED:START
See MIXED:END
See MIXED:CHAIN")
  
  (function allocate-buffer
    "Retrieves an unused buffer or allocates a new one if necessary.

Note: you MUST call FREE-BUFFER on the returned buffer once it is no
longer used, or you will leak memory.

See SERVER
See FREE-BUFFER
See MIXED:BUFFER")
  
  (function allocate-unpacker
    "Retrieves an unused unpacker or allocates a new one if necessary.

Note: you MUST call FREE-UNPACKER on the returned instance once it is
no longer used, or you will leak memory.

See SERVER
See FREE-UNPACKER
See MIXED:UNPACKER")
  
  (function free-buffer
    "Returns the buffer to the pool of free buffers, ready to be re-used.

Note that the memory held by the buffer is not freed until the server
itself is fully freed.

See SERVER
See ALLOCATE-BUFFER")
  
  (function free-unpacker
    "Returns the instance to the pool of free unpackers, ready to be re-used.

Note that the memory held by the unpacker is not freed until the server
itself is fully freed.

See SERVER
See ALLOCATE-UNPACKER")
  
  (function segment
    "Accesses the segment with the given name.

The index is managed via EQUAL, so both strings, symbols, etc. may be
used to name segments.

If T is used as the store, the current *SERVER* instance is used.

May also be used to search through MIXED:CHAINs.

See SERVER
See MIXED:CHAIN
See MIXED:SEGMENT")
  
  (function started-p
    "Returns T if the server is currently alive and running.

Note that a freed server will always be stopped, but a stopped server
has not necessarily been freed.

See SERVER")
  
  (function run-task
    "Function called around tasks that are executed in the server's render thread.

The TASK must be funcallable.

See SERVER
See CALL-IN-MIXING-CONTEXT")
  
  (function run
    "Function called within the server's render thread.

This function should control the rendering of the audio server. To do
so, it must repeatedly call MIXDE:MIX on the server and process any
tasks that may appear on the server's processing queue.

When this function returns, the server's segment must shut down and
the server must enter the stopped state.

See SERVER")
  
  (function call-in-mixing-context
    "Schedules FUNCTION to be called within SERVER's render thread.

If SYNCHRONIZE is T, this function will not return until the FUNCTION
has executed. In this case, the FUNCTION's return values will be
returned from this function. If SYNCHRONIZE is T and TIMEOUT is given,
the call will only block until at most TIMEOUT has passed, after which
NIL is returned.

If this function is called within the processing of the task queue,
the FUNCTION may be called immediately and synchronously.

If the server's task queue is full (which indicates the server has
shut down and is no longer processing), a WARNING is signalled, with
two restarts active: ABORT, to abort trying to execute FUNCTION, and
CONTINUE, to yield and continue trying to schedule. If the signal is
not handled, the function is not executed.

See SERVER
See RUN-TASK")
  
  (function with-server
    "Shorthand macro to run BODY within the render thread.

See CALL-IN-MIXING-CONTEXT")
  
  (function dot-server
    "Produces a graphviz file describing the server's segment layout.

Useful for debugging. If CONVERT is passed, invokes `dot` on the
produced dotfile to render it to the requested image type.

See SERVER"))

;; simple.lisp
(docs:define-docs
  (function detect-platform-drain
    "Returns the class name of the type of drain to use on this platform.

May return NIL if no suitable drains are present, though most likely
will simply return the dummy drain in that case.

This will *not* load any extra code, so it will only be able to try
out drain types that have been loaded in. It's up to you to load
modules you want to support.

See the cl-mixed extensions for supported systems.")
  
  (function make-simple-server
    "Convenience function to construct a server.

This function will compose a server with a mixing graph, ready for
use.

DEVICE is a hint passed on to the output drain to select a specific
output device. Usually a string of some kind, though the semantics are
drain-dependent.

LATENCY (in seconds) is used to compute the standard buffer
size. Setting this to 0.02 or so should be safe.

OUTPUT-CHANNELS is the number of channels of the internal mixing
hierarchy. Note that the drain may still perform conversion to upmix
or downmix regardless of this setting.

EFFECTS is a list of effects specs:

  EFFECT ::= class-designator | (class-name initarg*)

Each of the effects is applied in the same order as described and
applies to the audio stream right before output to the drain. Harmony
will automatically take care of matching up channel counts and
duplicating effects in BUNDLEs if necessary to upmix.

MIXERS is a list of mixer specs describing the mixers that are
available on the server. All of the mixers are ultimately combined
into one stream before output. The mixer specs are as follows:

  MIXER ::= name | (name class-designator [:effects (EFFECT*)] initarg*)

Effects here are as described above, applied to the output buffers of
the mixer.

Aside from the specified mixers, the server will always have a mixer
named :MASTER. It will also always have two named chains, :OUTPUT and
:SOURCES.

Returns the constructed SERVER.

See SERVER")
  
  (function maybe-start-simple-server
    "Constructs and starts a simple server if needed.

If *SERVER* is unbound, constructs a new one, using the passed
initargs and MAKE-SIMPLE-SERVER.
If *SERVER* is stopped, starts it.

See MAKE-SIMPLE-SERVER
See MIXED:START")
  
  (function play
    "Plays a source back.

SOURCE may be a source segment, file, or other descriptor for an audio
source that can be coerced into a VOICE.

MIXER designates the mixer to which the resulting voice is attached to
be played back.

LOCATION and VELOCITY may be location and velocity specs to be used
with a spatial mixer.

IF-EXISTS may be

  NIL        --- NIL is returned.
  :IGNORE    --- The existing voice is returned.
  :ERROR     --- An error is signalled.
  :RESTART   --- The voice is seeked to the beginning.
  :STOP      --- The voice is stopped and returned.
  :SUPERSEDE
  :REPLACE   --- The existing voice is stopped, removed, and a new
                 voice created.

This option is only used if a source with the same name already
exists on the server. Further, only :IGNORE and NIL are used if SOURCE
is a VOICE or name for a segment.

If RESET is non-NIL, the voice is seeked to the first sample.

If a new voice has to be created, it is done via CREATE, passing along
the matching arguments.

Returns the VOICE representing the SOURCE.

See VOICE
See WITH-SERVER
See CREATE")
  
  (function create
    "Creates a new voice to handle playback of a source.

CLASS should be a class designator for the voice to be constructed.

MIXER is used to ensure that the created voice has a matching channel
count to what is expected on the mixer.

IF-EXISTS may be

  NIL        --- NIL is returned.
  :IGNORE    --- The existing voice is returned.
  :ERROR     --- An error is signalled.
  :RESTART   --- The voice is seeked to the beginning.
  :STOP      --- The voice is stopped and returned.
  :SUPERSEDE
  :REPLACE   --- The existing voice is stopped, removed, and a new
                 voice created.

This option is only used if a source with the same name already exists
on the server.

Arguments other than NAME, CLASS, MIXER, SERVER, VOLUME, and
IF-EXISTS are passed on as initargs for the construction of the voice
(or whatever is supplied in CLASS).

Returns the constructed and started VOICE.

See PLAY
See VOICE")
  
  (function voices
    "Returns a fresh list of active voices playing back on the server.

If T is passed, the current *SERVER* is used.

See *SERVER*")
  
  (function clear
    "Removes and frees all sources from the server.

See VOICES
See *SERVER*"))

;; toolkit.lisp
(docs:define-docs
  (function add-to
    "Convenience function to quickly MIXED:ADD several instances.

See MIXED:ADD"))

;; voice.lisp
(docs:define-docs
  (type voice
    "Representation of a source, unpacker, and effects chain in one.

Accepts the following additional initargs on construction:

  :SOURCE
    Used to construct the source that's played back with this
    voice. See MAKE-SOURCE-FOR

  :EFFECTS
    Designates a list of effects to be applied to the output of the
    source. See MAKE-SIMPLE-SERVER for a description of the syntax.

  :CHANNELS
    The number of output channels. Used to downmix or upmix the source
    to match with the required output.

  :ON-END
    This option is available to all SOURCEs, but has additionally
    recognised options for VOICES:
      :FREE
        FREE is called on the voice.
      :DISCONNECT
        The voice is disconnected, unhooked from the server, and
        the source seeked to the beginning.
      :CALL-TRACK-END
        TRACK-END is called on the voice and source.
    You may still pass a function, which will receive the VOICE as
    argument.

Note that the voice can be directly manipulated \"as expected\"
through most CL-MIXED functions. Wrapper methods exist that will defer
to the appropriate parts of the voice. Control of specific effects
still requires retrieving the effects instances (through SEGMENT) and
manually setting their parameters. However, even parameters on the
connected mixer, such as MAX-DISTANCE, may be set directly on the
voice for convenience.

See SEGMENT
See SOURCE
See TRACK-END
See MAKE-SOURCE-FOR
See MAKE-SOURCE-FOR-PATH-TYPE
See SOURCE
See STOP
See MAKE-SIMPLE-SERVER
See MIXER
See CONNECT
See DISCONNECT
See REPEAT
See ACTIVE-P
See MIXED:FREE
See MIXED:ADD
See MIXED:OUTPUTS
See MIXED:OUTPUT
See MIXED:DONE-P
See MIXED:MIN-DISTANCE
See MIXED:MAX-DISTANCE
See MIXED:ROLLOFF
See MIXED:LOCATION
See MIXED:VELOCITY
See MIXED:SEEK-TO-FRAME
See MIXED:SEEK
See MIXED:FRAME-POSITION
See MIXED:SAMPLERATE")
  
  (function make-source-for
    "Creates a source segment for the given source type and initargs.

May be extended with further methods, but supports the following types
by default:

  PATHNAME
    The pathname is converted to a source via MAKE-SOURCE-FOR-PATH-TYPE
    by using the pathname-type as a keyword for the type.
  SOURCE
    The source is used directly.

See MAKE-SOURCE-FOR-PATH-TYPE")
  
  (function make-source-for-path-type
    "Attempts to construct a source that can handle the given type of file.

Note that this *may* load new code if quicklisp is available. However,
it is *heavily* recommended that you instead load all systems that you
require to decode source types yourself ahead of time to avoid bad
surprises on deployed systems or lag spikes during runtime.

See the cl-mixed extensions for supported formats.")

  (function track-end
    "Function called when voice ends.

See VOICE")
  
  (function source
    "Returns the SOURCE segment of the voice.

See VOICE")
  
  (function stop
    "Stops the voice.

This disconnects it from the server, removes it from processing, and
clears all buffers, ensuring no leftover playback samples.

See VOICE"))
