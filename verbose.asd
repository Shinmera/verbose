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
  :version "0.9.3"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A logging framework using the piping library."
  :serial T
  :components ((:file "package")
               (:file "controller")
               (:file "message")
               (:file "pipes")
               (:file "default"))
  :depends-on (:piping
               :local-time
               :bordeaux-threads
               :split-sequence
               :cl-fad
               :clon))

;; (defsystem verbose-doc
;;   :name "Verbose Doc"
;;   :components ((:file "documentation"))
;;   :depends-on (:verbose :lquery-doc))
