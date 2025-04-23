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
             (bt:thread-alive-p (thread controller))
             (not (eq (thread controller) (bt:current-thread))))
    (let ((sync (make-sync-request)))
      (bt:with-lock-held ((sync-request-lock sync))
        (pass controller sync)
        (bt:condition-wait (sync-request-condition sync) (sync-request-lock sync))))))

(defun flush (&optional (controller *global-controller*))
  (cond ((null (thread controller)))
        ((eq (thread controller) (bt:current-thread))
         (process-message-batch (queue controller) (pipeline controller)))
        (T
         (sync controller)))
  ;; TODO: do this for *all* segments
  (when (find-place controller 'repl-faucet)
    (finish-output (output (find-place controller 'repl-faucet)))))
