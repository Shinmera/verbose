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
  (:export :log :fatal :severe :error :warn :info :debug :trace
           :*global-controller* :controller :remove-global-controller :make-standard-global-controller
           :attach-to :set-repl-level
           :*repl-faucet-timestamp*
           :repl-faucet :format-message
           :cron-interval :make-cron-interval
           :rotating-log-faucet :rotate-log :update-interval))
