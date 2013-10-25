#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.verbose.asd
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.verbose.asd)

(defsystem verbose
  :name "Verbose"
  :version "0.0.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A logging library designed for use in Radiance."
  :serial T
  :components ((:file "package.lisp")
               (:file "documentation.lisp")
               (:file "controller.lisp"))
  :depends-on (:local-time
               :bordeaux-threads))
