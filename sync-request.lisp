(in-package #:org.shirakumo.verbose)

(defstruct sync-request
  (condition (bt:make-condition-variable))
  (lock (bt:make-lock)))

(defmethod pass ((vector vector) (sync sync-request))
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
