#|
 This file is a part of Verbose
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defvar *verbose-conditions* T)

(defclass condition-message (message)
  ((condition :initarg :condition :accessor message-condition))
  (:default-initargs
   :condition (cl:error "CONDITION required.")))

(defmethod log (level categories (datum condition) &rest args)
  (declare (ignore args))
  (let ((content (if *verbose-conditions*
                     (with-output-to-string (stream)
                       (dissect:present condition stream))
                     (princ-to-string condition))))
    (log-message level categories content 'condition-message :condition condition)))
