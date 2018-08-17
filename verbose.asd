#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem verbose
  :name "Verbose"
  :version "2.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A logging framework using the piping library."
  :homepage "https://Shinmera.github.io/verbose/"
  :bug-tracker "https://github.com/Shinmera/verbose/issues"
  :source-control (:git "https://github.com/Shinmera/verbose.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "controller")
               (:file "message")
               (:file "pipes")
               (:file "convenience")
               ;; Extensions
               (:file "conditions")
               (:file "muffling")
               (:file "sync-request")
               (:file "documentation"))
  :depends-on (:piping
               :local-time
               :bordeaux-threads
               :dissect
               :documentation-utils))
