#|
 This file is a part of Verbose
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defstruct sync-request
  (condition (bt:make-condition-variable))
  (lock (bt:make-lock)))

(defmethod pass ((array array) (sync sync-request))
  ;; Make sure it's actually waiting on the condition.
  (bt:with-lock-held ((sync-request-lock sync)))
  (bt:condition-notify (sync-request-condition sync)))

(defun sync (&optional (controller *global-controller*))
  (when (and controller (thread controller)
             (bt:thread-alive-p (thread controller)))
    (let ((sync (make-sync-request)))
      (bt:with-lock-held ((sync-request-lock sync))
        (pass controller sync)
        (bt:condition-wait (sync-request-condition sync) (sync-request-lock sync))))))
