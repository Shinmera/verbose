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
                       (dissect:present datum stream))
                     (princ-to-string datum))))
    (log-message level categories content 'condition-message :condition datum)))
