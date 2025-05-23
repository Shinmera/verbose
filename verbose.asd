(asdf:defsystem verbose
  :name "Verbose"
  :version "2.2.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A logging framework using the piping library."
  :homepage "https://Shinmera.github.io/verbose/"
  :bug-tracker "https://github.com/Shinmera/verbose/issues"
  :source-control (:git "https://github.com/Shinmera/verbose.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "controller")
               (:file "pool")
               (:file "message")
               (:file "pipes")
               (:file "convenience")
               ;; Extensions
               (:file "conditions")
               (:file "muffling")
               (:file "sync-request")
               (:file "documentation"))
  :depends-on (:piping
               :bordeaux-threads
               :atomics
               :dissect
               :documentation-utils))
