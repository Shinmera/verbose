#|
  This file is a part of XenCL
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage :org.tymoonnext.radiance.lib.verbose.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :verbose-doc)
  (:export :build-documentation))

(in-package :org.tymoonnext.radiance.lib.verbose.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr :name (symbol-name (nth 0 object)))))

(defun build-documentation ()
  (write-documentation :verbose
                       (merge-pathnames "about-template.html" (asdf:system-source-directory :verbose))
                       :output-file (merge-pathnames "about.html" (asdf:system-source-directory :verbose))
                       :exclude '(:internal)))
