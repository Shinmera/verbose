#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :cl)
(defpackage org.tymoonnext.radiance.lib.verbose
  (:nicknames :verbose :v)
  (:use :cl :bordeaux-threads :piping :split-sequence)
  (:shadow LOG ERROR WARN DEBUG TRACE)
  ;; controller.lisp
  (:export
   #:*global-controller*
   #:controller
   #:pass
   #:shared-instance)
  ;; default.lisp
  (:export
   #:make-standard-global-controller
   #:remove-global-controller
   #:restart-global-controller
   #:set-repl-level
   #:set-repl-categories
   #:add-repl-category
   #:remove-repl-category)
  ;; message.lisp
  (:export
   #:log-object
   #:message
   #:message-time
   #:message-thread
   #:message-level
   #:message-category
   #:message-content
   #:log-message
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
   #:make-cron-interval
   #:rotating-log-faucet
   #:time-format
   #:faucet-file
   #:current-file
   #:interval
   #:rotate-log
   #:update-interval
   #:stop-rotation
   #:category-filter
   #:categories
   #:category-tree-filter))
