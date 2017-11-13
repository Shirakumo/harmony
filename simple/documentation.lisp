#|
 This file is a part of harmony
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.harmony.simple)

(docs:define-docs
  (variable *server*
    "This variable holds the currently active SERVER instance.

See HARMONY:SERVER
See INITIALIZE")

  (type default-server
    "Subclass for the default server used in harmony-simple.

On initialize-instance this server constructs its own
pipeline by using MAKE-PIPELINE and a successive
COMPILE-PIPELINE.

See HARMONY:SERVER
See OUTPUT-SPEC
See MAKE-PIPELINE
See HARMONY:COMPILE-PIPELINE")

  (function output-spec
    "Accessor to the list of initargs used to construct the output drain for the server.

This list should consist of a class designator and
the respective make-instance initargs.

See DEFAULT-SERVER")

  (function make-segment
    "Create an instance of a segment.

This is a wrapper around MAKE-INSTANCE, making sure to
supply the current *SERVER* as the :context initarg.

See *SERVER*")

  (function make-pipeline
    "Constructs a pipeline for the given server.

See HARMONY:SERVER
See HARMONY:PIPELINE")

  (function ensure-segment
    "Coerces the segment-ish to a segment if possible.

SEGMENT-ISH may be either a:
  HARMONY:SEGMENT --- The argument is returned as-is.
  SYMBOL          --- SEGMENT is used to find the segment
                      on the server. If no such segment
                      can be found, an error is signalled.

See SEGMENT
See HARMONY:SEGMENT")

  (function play
    "Wrapper around HARMONY:PLAY to use the default server.

See HARMONY:PLAY")

  (function decode
    "Wrapper around HARMONY:DECODE to make sure the samplerate matches with the server.

See HARMONY:DECODE")

  (function initialize
    "Constructs and starts the harmony server.

This sets the current value of *SERVER*, unless
*SERVER* is already non-NIL, in which case it does
nothing.

See DEFAULT-SERVER
See *SERVER*")

  (function start
    "Starts the server or the given thing.

See HARMONY:START")

  (function started-p
    "Returns whether the server or the given thing is started.

See HARMONY:STARTED-P")

  (function stop
    "Stops the server or the given thing.

See HARMONY:STOP")

  (function pause
    "Pauses the server or the given thing.

See HARMONY:PAUSE")

  (function paused-p
    "Returns whether the server or the given thing is paused.

See HARMONY:PAUSED-P")

  (function resume
    "Resumes the server or the given thing.

See HARMONY:RESUME")

  (function segment
    "Returns the segment in the server with the given name.

If no such segment can be found, NIL is returned instead."))
