(in-package #:org.shirakumo.verbose)

;; conditions.lisp
(docs:define-docs
  (variable *verbose-conditions*
    "Whether to print a full stack trace with a condition.

See DISSECT:PRESENT")

  (type condition-message
    "Message class to handle conditions being logged.

See *VERBOSE-CONDITIONS*
See MESSAGE-CONDITION")

  (function message-condition
    "Accessor to the condition that the message carries.

See CONDITION-MESSAGE"))

;; controller.lisp
(docs:define-docs
  (variable *global-controller*
    "Stores the default controller.

While it is possible to have multiple controllers, there is not
much reason to do so. The pipeline system should be flexible enough
to bypass the need for multiple controllers. However, you can still
instantiate multiple controllers and manage them in whatever way you
like. Just make sure to bind this variable to the appropriate one
if the function does not accept the controller as an argument
directly.")

  (variable *process-locally*
    "Whether to process messages in the current thread.

If this is T, then the messages will be shot through the pipeline
in the thread whether the log statement is issued. This may be
useful on systems where thread support is not necessary, but will
most likely be less performant.

If you really do not need to use threads, it is probably a better
idea to simply shut down the controller's thread anyway. You can
do this with STOP.")

  (type controller
    "This is the central piece that handles log messages.

By default a background thread is spawned that processes the
messages. You can control the thread with START and STOP and
enqueue messages with PASS. You can prevent the controller
from being automatically STARTed by passing :DONT-START T as
an initarg.

See THREAD
See THREAD-CONTINUE
See QUEUE
See QUEUE-BACK
See QUEUE-CONDITION
See QUEUE-LOCK
See START
See STOP
See PIPING:PASS")

  (function thread
    "Accessor to the thread held by the object.

See CONTROLLER
See MESSAGE")

  (function thread-continue
    "Whether the controller thread should continue or not.

See CONTROLLER")

  (function queue
    "The vector for the front message queue.

In order to add messages to this queue, you should use 
VECTOR-PUSH-EXTEND wrapped in a WITH-CONTROLLER-LOCK.

See CONTROLLER")

  (function queue-back
    "The back queue for the controller.

This is swapped with the front QUEUE when the controller decides
to start processing messages.

See CONTROLLER")

  (function queue-condition
    "The condition variable the controller will wait on until signalled.

See CONTROLLER")

  (function queue-lock
    "The lock to mutually exclude the controller thread and other threads from accessing the queues simultaneously.

See CONTROLLER")

  (function start
    "Starts the background thread of the controller.

If there is no thread support, this does nothing.
If there is already a thread active on the controller, a continuable
error is signalled.

See CONTROLLER")

  (function stop
    "Stops the background thread of the controller.

If there is no thread support, this does nothing.
This function may take up to ~0.5s as it waits for the thread to
exit on its own. If it still fails then, it will call 
BT:DESTROY-THREAD and hope that it will terminate properly.

See CONTROLLER")

  (function with-controller-lock
    "Evaluates FORMS with the controller's lock held.

This will block until it can acquire the lock.

See QUEUE-LOCK")

  (function controller-loop
    "Main function of the controller's background thread. Should handle message processing.

This function should return when THREAD-CONTINUE is set to NIL.

See CONTROLLER"))

;; convenience.lisp
(docs:define-docs
  (function remove-global-controller
    "Stops and entirely removes the global controller, if present.

See STOP
See *GLOBAL-CONTROLLER*")

  (function restart-global-controller
    "Replaces the current global controller (if any) with a freshly allocated one.

In case things really messed up somehow, this may resolve your problems
at the cost of throwing away messages that are still queued up, and
restoring the standard pipeline.

See REMOVE-GLOBAL-CONTROLLER
See *GLOBAL-CONTROLLER*
See MAKE-STANDARD-GLOBAL-CONTROLLER")

  (function repl-level
    "Accessor to the current logging level that should be output to the REPL.

See LEVEL-FILTER
See *LEVELS*")

  (function repl-categories
    "Accessor to the categories that will be printed at the REPL.

See CATEGORY-TREE-FILTER
See ADD-REPL-CATEGORY
See REMOVE-REPL-CATEGORY")

  (function add-repl-category
    "Convenience function to add more categories.

The first argument may be the controller object to modify.

See REPL-CATEGORIES")

  (function remove-repl-category
    "Convenience function to remove repl categories.

The first argument may be the controller object to modify.

See REPL-CATEGORIES")

  (function output-here
    "Modifies the standard repl faucet to output to the given stream.

Also useful when the *STANDARD-OUTPUT* changes, such as in the
case of SWANK and multiple connections to the same instance.

See REPL-FAUCET")

  (function add-pipe
    "Adds a new pipe to the controller.

The first argument may be the controller object to modify.

Constructs a pipe with the given segments as per MAKE-PIPE and
INSERT, then finally adds the segment to the controller by
ADD-SEGMENT.")

  (function define-pipe
    "Shorthand convenience macro to define new pipes.

SEGMENT ::= (class-name NAME? INITARG*)
NAME    ::= :name symbol
INITARG ::= keyword value

The optional name if given will be automatically assigned to
the pipe segment, so that it can be retrieved through FIND-PLACE

See PIPING:FIND-PLACE")

  (function make-standard-global-controller
    "Constructs a new controller object with a single pipe in the pipeline

The pipe has a level-filter, category-tree-filter, and repl-faucet.

All arguments to this function are simply passed as initargs to
the construction of the CONTROLLER instance.

See LEVEL-FILTER
See CATEGORY-TREE-FILTER
See REPL-FAUCET
See CONTROLLER"))

;; message.lisp
(docs:define-docs
  (variable *levels*
    "Holds the alist of priorities to level names.

This list must be sorted in ascending order of priorities.

See ADD-LEVEL
See DEFINE-LEVEL")

  (variable *timestamp-format*
    "The format in which timestamps will be formatted for messages.

The format should be a list of specs, each of which can be:
  STRING
  CHARACTER
  (THING LENGTH) --- Where THING can be one of:
    :year :y
    :month :m
    :day :d
    :hour :h :hh
    :min :mm
    :sec :s :ss")

  (variable *default-message-class*
    "The class designator of the message class to use when logging messages.")

  (function log-object
    "Passes the given object to the global controller if it exists.

See *GLOBAL-CONTROLLER*
See PIPING:PASS")

  (type message
    "The base class for most logging messages that will be sent through the framework.

See TIMESTAMP
See THREAD
See LEVEL
See CATEGORIES
See CONTENT
See FORMAT-MESSAGE
See LOG-MESSAGE")

  (function timestamp
    "The universal-time timestamp recorded from the time of issuing of the message.

See MESSAGE")

  (function level
    "The level at which the object operates.

See *LEVELS*
See MESSAGE
See LEVEL-FILTER")

  (function categories
    "The categories against which the object operates.

See MESSAGE
See CATEGORY-FILTER")

  (function content
    "The primary content that the message carries.

Usually when the message is formatted, this will be printed by PRINC.

See FORMAT-MESSAGE
See MESSAGE")

  (function format-message
    "Formats the message according to the given thing.

Standard implementations exist for NULL and STREAM.
NULL will always return a string, and STREAM will print the
message to the stream.

You will want to extend or override methods on this to change
the way messages are presented.

See MESSAGE")

  (function log-message
    "Constructs a logging message and sends it off to be logged.

See *DEFAULT-MESSAGE-CLASS*
See LOG-OBJECT")

  (function log
    "Causes a message to be logged as appropriate for the given datum.

By default, methods for STRING, SYMBOL, FUNCTION, and T exist.
  STRING   --- The string is taken as a format string and the args
               as format arguments.
  SYMBOL   --- The symbol is taken as condition or class name and
               a condition/class is constructed with the args as
               initargs.
  FUNCTION --- The function is wrapped in a lambda as a call with
               the arguments. This will then cause the function to
               get called during the processing of the message
               when FORMAT-MESSAGE is called.
  T        --- The arguments are discarded and the datum is used
               as the CONTENT of the message.

See LOG-MESSAGE")

  (function add-level
    "Adds or updates a level with the given priority and level.

If a level with the requested name already exists, its
priority is updated. Otherwise a new level is added.

See *LEVELS*")

  (function define-level
    "Defines a new level by adding it and constructing a helper function.

The helper function will automatically be exported, if its name
is in the VERBOSE package (as it is by default).

See ADD-LEVEL"))

;; muffling.lisp
(docs:define-docs
  (variable *muffled-categories*
    "Variable containing a list of categories that should get muffled upon issuing.

If one of the categories is T, all messages are muffled.
When a category is muffled, it is effectively removed from the
message. If a message has no categories whatsoever, it is not
issued.

See WITH-MUFFLED-LOGGING
See WITH-MUFFLED-LOGGING*")

  (function with-muffled-logging
    "Adds the requested categories to the list of muffled categories within the body.

This change is only visible to the current thread and within the
execution context of BODY.

If CATEGORY is NIL, this acts as an override instead and only the
listed categories are muffled.

See *MUFFLED-CATEGORIES*
See WITH-MUFFLED-LOGGING*")

  (function with-muffled-logging*
    "Adds the requested categories to the list of muffled categories.

Unlike WITH-MUFFLED-LOGGING the change to the categories will be
visible to all threads that didn't bind *MUFFLED-CATEGORIES*.

If CATEGORY is NIL, this acts as an override instead and only the
listed categories are muffled.

See *MUFFLED-CATEGORIES*
See WITH-MUFFLED-LOGGING"))

;; pipes.lisp
(docs:define-docs
  (type stream-faucet
    "A faucet that prints the messages it receives to a stream.

Messages are first passed through FORMAT-MESSAGE on the
faucet itself, and then on the output stream.

See OUTPUT
See PIPING:FAUCET")

  (function output
    "The stream to which the stream-faucet outputs the messages.

See STREAM-FAUCET")

  (type repl-faucet
    "A faucet that prints to the *standard-output*.

See REPL-FAUCET")

  (type file-faucet
    "A faucet that outputs to a file.

See FILE
See STREAM-FAUCET")

  (function file
    "Accessor to the file to which the faucet is currently outputting.")

  (type rotating-log-faucet
    "A faucet that outputs to a file and rotates on an interval.

Instead of FILE you should pass TEMPLATE as an initarg. The
date and time of the rotation are prepended to the template
pathname.

Note that the rotation is only potentially done when a message
is passed to the faucet. This avoids having to use synchronisation
and background threads.

The interval can be one of :HOURLY :DAILY :MONTHLY :WEEKLY.
:HOURLY :DAILY and :MONTHLY will rotate as close to their
\"zero\" as possible. :WEEKLY will always rotate exactly a
week from the previous rotation the earliest.

See INTERVAL
See LAST-ROTATION
See TEMPLATE
See ROTATE
See FILE-FAUCET")

  (function interval
    "The interval in which the faucet's file is rotated.

Can be one of :HOURLY :DAILY :MONTHLY :WEEKLY

See ROTATING-LOG-FAUCET")

  (function last-rotation
    "The universal-time timestamp of the last rotation.

See ROTATE
See ROTATING-LOG-FAUCET")

  (function template
    "The pathname serving as a template for the rotated files.

See ROTATING-LOG-FAUCET")

  (function rotate
    "Causes the faucet to rotate to a new file immediately.

See ROTATING-LOG-FAUCET")

  (type level-filter
    "Only lets through messages that are of the given level or higher.

LEVEL can be either the name of a level in *LEVELS* or an
integer of the appropriate size.

See LEVEL
See PIPING:FILTER")

  (type category-filter
    "Only lets through messages that contain a category in the filter's categories list.

CATEGORIES can be T in which case all messages are let through.

See CATEGORIES
See PIPING:FILTER")

  (type category-tree-filter
    "Only lets through messages that match a category in the filter's category list.

See MATCHING-TREE-CATEGORY
See CATEGORY-FILTER")

  (function matching-tree-category
    "This returns T if

FILTER and CATEGORY are EQL or
Both can be turned into sequences of items by splitting
their SYMBOL-NAMEs on every occurrence of a period and
each item in the list made from FILTER occurs in the same
order at the beginning of the list made from CATEGORY.

More simply put, if category is a sub-branch of filter,
it passes the test."))

;; sync-request.lisp
(docs:define-docs
  (type sync-request
    "Struct to hold a request for synchronisation.

When this object is processed by being passed down a vector
it will first acquire and immediately release its lock and
then notify its condition variable. The lock serves as a
rendezvous point.

See SYNC-REQUEST-CONDITION
See SYNC-REQUEST-LOCK
See SYNC")

  (function sync-request-condition
    "Accessor to the condition variable for the sync request.

See SYNC-REQUEST")

  (function sync-request-lock
    "Accessor to the lock for the sync request.

See SYNC-REQUEST")

  (function sync
    "Blocks the current thread until all messages before this point have been processed.

If the controller is not running a thread, this does nothing.
Otherwise this is achieved by constructing and then passing
a SYNC-REQUEST instance to the controller.

See SYNC-REQUEST
See FLUSH")

  (function flush
    "Similar to SYNC but tries to ensure all messages have been flushed.

Also flushes the streams of all segments.

See SYNC"))

;; toolkit.lisp
(docs:define-docs
  (function removef
    "Constructs a copy of the plist where the key-value pairs in keys are not included.")

  (function split
    "Splits the STRING into a list of strings where ON is taken as the character to split by."))
