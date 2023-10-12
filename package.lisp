(in-package #:cl-user)
(push :verbose *features*)
(defpackage #:org.shirakumo.verbose
  (:use :cl :piping)
  (:shadow #:log #:error #:warn #:debug #:trace)
  ;; conditions.lisp
  (:export
   #:condition-message
   #:message-condition)
  ;; controller.lisp
  (:export
   #:*global-controller*
   #:*process-locally*
   #:controller
   #:thread
   #:thread-continue
   #:queue
   #:queue-back
   #:queue-condition
   #:queue-lock
   #:start
   #:stop
   #:with-controller-lock
   #:controller-loop)
  ;; convenience.lisp
  (:export
   #:remove-global-controller
   #:restart-global-controller
   #:repl-level
   #:repl-categories
   #:add-repl-category
   #:remove-repl-category
   #:output-here
   #:add-pipe
   #:define-pipe
   #:make-standard-global-controller)
  ;; message.lisp
  (:export
   #:*levels*
   #:*timestamp-format*
   #:*default-message-class*
   #:log-object
   #:message
   #:timestamp
   #:thread
   #:level
   #:categories
   #:content
   #:format-message
   #:log-message
   #:log
   #:add-level
   #:define-level)
  ;; muffling.lisp
  (:export
   #:*muffled-categories*
   #:with-muffled-logging)
  ;; pipes.lisp
  (:export
   #:stream-faucet
   #:output
   #:repl-faucet
   #:ansi-colors
   #:file-faucet
   #:file
   #:rotating-file-faucet
   #:interval
   #:last-rotation
   #:template
   #:rotate
   #:level-filter
   #:level
   #:category-filter
   #:categories
   #:category-tree-filter
   #:matching-tree-category)
  ;; sync-request.lisp
  (:export
   #:sync-request
   #:sync-request-condition
   #:sync-request-lock
   #:sync))
