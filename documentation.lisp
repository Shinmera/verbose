#|
 This file is a part of Verbose
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

;; controller.lisp
(docs:define-docs
  (variable *global-controller*
    "Global variable holding the current verbose controller instance and pipeline.")

  (variable *muffled-categories*
    "Which categories of messages that are handed to PASS to muffle (not put onto the pipeline).")

  (variable *process-locally*
    "Whether to process messages in the local thread rather than the controller thread.")

  (type controller
    "Main controller class that holds the logging pipeline, thread and shares.")
  
  (function start-controller
    "Starts the controller's thread.")

  (function stop-controller
    "Stops the controller's thread.")

  (function with-controller-lock
    "Wraps the body into an environment with the thread lock for the controller acquired so it is safe to modify the pipeline or controller slots.")

  (function share
    "Accessor to the value of a shared special variable.

Shared bindings are special variable bindings that are active during the message
passing in the controller's logging thread. This allows you to influence various
behaviours of the pipeline, such as where and how the messages are printed.

This is thread-safe.")

  (function shared-bindings
    "Returns a cons of two lists, the first list containing the binding names and the second the values.
For use with PROGV.

See WITH-SHARES.")

  (function with-shares
    "Binds all shared special variables of CONTROLLER within BODY.

See SHARE.")

  (function copy-bindings
    "Copy all the values of VARIABLEs over to shared bindings in the CONTROLLER.

See (SETF SHARE).")

  (function set-standard-special-values
    "Set all standard special values from CL as shared bindings.

See COPY-BINDINGS.")

  (function pass
    "Pass a raw message into the controller pipeline. 

Note that this is not instant as the message passing is done in a separate thread.
This function returns as soon as the message is added to the queue, which should be
near-instant.

In the case that no live thread is available on the controller, either due to a lack
of thread support entirely, or some other factor, the message is passed along to the
pipeline directly. This will mean it is processed immediately, but it also means it
will hold up the current thread.

Also note that, depending on *MUFFLED-CATEGORIES*, some messages may not actually
be put onto the pipeline at all.")

  (function with-muffled-logging
    "Muffles all messages of CATEGORY within BODY.
This means that all logging statements that fit CATEGORY within the BODY
are not actually ever handed to the pipeline. If CATEGORY is T, all
messages are muffled.")

  (function sync
    "Causes a sync message to be sent to the controller thread (if present).
This will only return once the sync message has been processed by the controller thread.
You can use this in order to ensure that the logging messages you've sent out before
calling sync have been processed."))

;; default.lisp
(docs:define-docs
  (function make-standard-global-controller
    "Creates a new standard global controller construct with a setup pipeline.")

  (function remove-global-controller
    "Attempts to destroy the thread of the controller and remove it.")

  (function restart-global-controller
    "Removes the controller and creates a new one in its place.")

  (function repl-level
    "Accesses the current logging level of the standard repl faucet.")

  (function repl-categories
    "Accesses the list of allowed log categories of the standard category-tree-filter.
If NIL is returned, anything is passed.")

  (function add-repl-category
    "Add a new repl category to allow.")

  (function remove-repl-category
    "Remove an existing repl category.")

  (function output-here
    "Set the *standard-output* shared instance to STANDARD-OUTPUT, effectively
redirecting output from the logger thread to the specified stream.")

  (function add-pipe
    "Add a new pipe with the given segments."))

;; message.lisp
(docs:define-docs
  (variable *levels*
    "List of accepted logging level names. The position in the list defines its level, ranking from finest to roughest.")

  (variable *verbose-conditions*
    "Wether to report conditions verbosely using DISSECT:PRESENT.")

  (function log-object
    "Shorthand for (pass *global-controller* object)")

  (type message
    "Message object to be passed around the system.")

  (type condition-message
    "Message object that carries a condition object.")

  (function message-visible
    "Returns T if the message is visible under the given level.")

  (function log-message
    "Logs a message with the given parameters.")

  (function log
    "Logs a new message under given level and categories, using the datum and datum-args to construct the content.

The following datum classes are recognised by default:
STRING     -- Construct the content through FORMAT with datum and datum-args.
SYMBOL     -- If datum names a condition class, MAKE-CONDITION is called with
              the datum and datum-args. Otherwise MAKE-INSTANCE is used. LOG
              is subsequently called /again/ with the resulting instance as
              the new datum. This ensures that dispatching happens as usual.
FUNCTION   -- Use a (lambda () (apply datum datum-args)) as the content.
              By default this means that the given function will be called
              within the controller thread. See FORMAT-MESSAGE. Be aware
              that this means the function will be executed at least once
              for each faucet, and potentially arbitrary times to perform
              the pipelinging.
T          -- Discard the datum-args and use datum directly as content.")

  (function define-level
    "Define a new shorthand function for logging under a specific level."))

;; pipes.lisp
(docs:define-docs
  (variable *repl-faucet-timestamp*
    "Default timestamp format of the repl faucet. See LOCAL-TIME:FORMAT-TIMESTRING.")

  (function format-message
    "Wrapper around pass to potentially be overwritten if the output format should be changed.
This serves a dual purpose: First it allows writers of faucets to customise
their output representation. Secondly, it coerces the actual message content
into a string to output.

By default, the following methods are defined:
\(REPL-FAUCET MESSAGE) print to the repl, using (NIL content) to coerce the content.
\(STREAM MESSAGE)
\(NIL CONDITION)       if *VERBOSE-CONDITIONS* is T, (DISSECT:PRESENT message) is
used as the output. Otherwise, the message itself is returned
directly.
\(NIL FUNCTION)        call the message function and return its result.
\(NIL T)               simply return the message.")

  (type repl-faucet
    "A simple logging faucet that prints log messages to the *standard-output*")
  
  (type file-faucet
    "A faucet that writes everything to a file.")

  (type rotating-file-faucet
    "A file faucet that includes automatic log file rotation.")
  
  (type category-filter
    "A simple pipe filter that only lets through matching categories.")

  (type category-tree-filter
    "A pipe filter that only lets messages through whose category matches by tree.")

  (type level-filter
    "A simple pipe filter that only lets through messages that are above a certain level.")



  (function make-cron-interval
    "Parse a cron interval.")

  (type rotating-log-faucet
    "A file logger that rotates at the given (cron) interval.")

  (function rotate-log
    "Initiate a log rotation immediately. This does not influence the automatic rotation interval.")

  (function update-interval
    "Change the rotation interval. cron-interval should either be a cron-parsable string or a clon:cron-schedule.")

  (function stop-rotation
    "Stops the rotation. Logging will still continue on the current file."))
