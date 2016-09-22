#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem verbose
  :name "Verbose"
  :version "1.2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A logging framework using the piping library."
  :homepage "https://github.com/Shinmera/verbose"
  :serial T
  :components ((:file "package")
               (:file "controller")
               (:file "message")
               (:file "pipes")
               (:file "convenience")
               (:file "documentation"))
  :depends-on (:piping
               :local-time
               :bordeaux-threads
               :split-sequence
               :clon
               :dissect
               :documentation-utils))
