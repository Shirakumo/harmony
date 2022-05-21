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
  (function buffer
    "")
  
  (function from
    "")
  
  (function from-location
    "")
  
  (function to
    "")
  
  (function to-location
    "")
  
  (function segment
    "")
  
  (function name
    "")
  
  (function chain
    "")
  
  (function connect
    "")
  
  (function disconnect
    "")
  
  (function downstream
    "")
  
  (function upstream
    "")
  
  (function source
    "")
  
  (function repeat
    "")
  
  (function repeat-start
    "")
  
  (function on-end
    ""))
;; server.lisp
(docs:define-docs
  (function *server*
    "")
  
  (function server
    "")
  
  (function allocate-buffer
    "")
  
  (function allocate-unpacker
    "")
  
  (function free-buffer
    "")
  
  (function free-unpacker
    "")
  
  (function segment
    "")
  
  (function started-p
    "")
  
  (function run-task
    "")
  
  (function run
    "")
  
  (function call-in-mixing-context
    "")
  
  (function with-server
    "")
  
  (function dot-server
    ""))

;; simple.lisp
(docs:define-docs
  (function detect-platform-drain
    "")
  
  (function make-simple-server
    "")
  
  (function maybe-start-simple-server
    "")
  
  (function play
    "")
  
  (function create
    "")
  
  (function voices
    "")
  
  (function clear
    ""))

;; toolkit.lisp
(docs:define-docs
  (function add-to
    ""))

;; voice.lisp
(docs:define-docs
  (function voice
    "")
  
  (function make-source-for
    "")
  
  (function make-source-for-path-type
    "")
  
  (function source
    "")
  
  (function stop
    ""))
