#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(push :verbose *features*)
(defpackage #:verbose
  (:nicknames #:v #:org.shirakumo.verbose)
  (:use :cl :piping :split-sequence)
  (:shadow #:LOG #:ERROR #:WARN #:DEBUG #:TRACE)
  ;; controller.lisp
  (:export
   #:*global-controller*
   #:*muffled-categories*
   #:*process-locally*
   #:controller
   #:start-controller
   #:stop-controller
   #:with-controller-lock
   #:share
   #:with-shares
   #:copy-bindings
   #:set-standard-special-values
   #:pass
   #:shared-instance
   #:with-muffled-logging
   #:sync)
  ;; default.lisp
  (:export
   #:make-standard-global-controller
   #:remove-global-controller
   #:restart-global-controller
   #:repl-level
   #:repl-categories
   #:add-repl-category
   #:remove-repl-category
   #:output-here
   #:add-pipe)
  ;; message.lisp
  (:export
   #:*levels*
   #:*verbose-conditions*
   #:log-object
   #:message
   #:message-time
   #:message-thread
   #:message-level
   #:message-category
   #:message-categories
   #:message-content
   #:message-visible
   #:condition-message
   #:message-condition
   #:log-message
   #:log-condition
   #:log
   #:FATAL
   #:SEVERE
   #:ERROR
   #:WARN
   #:INFO
   #:DEBUG
   #:TRACE)
  ;; pipes.lisp
  (:export
   #:*repl-faucet-timestamp*
   #:format-message
   #:repl-faucet
   #:file-faucet
   #:faucet-file
   #:rotating-file-faucet
   #:interval
   #:last-rotation
   #:template
   #:rotate
   #:category-filter
   #:categories
   #:category-tree-filter
   #:level-filter)
  ;; backwards compat
  (:export
   #:make-cron-interval
   #:rotating-log-faucet
   #:time-format
   #:current-file
   #:interval
   #:rotate-log
   #:update-interval
   #:stop-rotation))
