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
   #:*controller-standard-output*
   #:*controller-standard-input*
   #:controller
   #:pass
   #:remove-global-controller)
  ;; default.lisp
  (:export
   #:make-standard-global-controller
   #:restart-global-controller
   #:attach-to
   #:set-repl-level
   #:set-repl-categories
   #:add-repl-category
   #:remove-repl-category)
  ;; message.lisp
  (:export
   #:log-object
   #:message
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
   #:rotate-log
   #:update-interval
   #:stop-rotation
   #:category-filter
   #:category-tree-filter))
