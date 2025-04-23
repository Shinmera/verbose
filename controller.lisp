(in-package #:org.shirakumo.verbose)

(defvar *global-controller* NIL)
(defvar *process-locally* NIL)

(defclass controller (pipeline)
  ((thread :initform NIL :accessor thread)
   (thread-continue :initform NIL :accessor thread-continue)
   (queue :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor queue)
   (queue-back :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor queue-back)
   (queue-condition :initform (bt:make-condition-variable :name "MESSAGE-CONDITION") :reader queue-condition)
   (queue-lock :initform (bt:make-lock "MESSAGE-LOCK") :reader queue-lock)))

(defmethod initialize-instance :after ((controller controller) &key dont-start)
  (unless dont-start (start controller)))

(defmethod print-object ((controller controller) stream)
  (print-unreadable-object (controller stream :type T)
    (format stream "~@[:threaded~*~]~@[ :running~*~] :queue-size ~d"
            (thread controller) (thread-continue controller) (length (queue controller)))))

(defmethod start ((controller controller))
  (setf (thread-continue controller) T)
  (when (thread controller)
    (cerror "Spawn a new thread anyway"
            "There is already a thread set on the controller."))
  #+:thread-support
  (setf (thread controller)
        (bt:make-thread (lambda () (controller-loop controller))
                        :name "verbose-thread"
                        :initial-bindings `((*global-controller* . ,controller)
                                            (*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)
                                            (*trace-output* . ,*trace-output*)
                                            (*query-io* . ,*query-io*)
                                            (*debug-io* . ,*debug-io*)))))

(defmethod stop ((controller controller))
  (setf (thread-continue controller) NIL)
  #+:thread-support
  (loop for thread = (thread controller)
        for i from 0
        while (and thread (bt:thread-alive-p thread))
        do (bt:condition-notify (queue-condition controller))
           (sleep 0.1)
           (when (< 5 i)
             (bt:destroy-thread thread)
             (return)))
  controller)

(defmacro with-controller-lock ((&optional (controller '*global-controller*)) &body forms)
  `(bt:with-lock-held ((queue-lock ,controller))
     ,@forms))

(defun process-message-batch (queue pipeline)
  (with-simple-restart (skip "Skip processing the message batch.")
    (loop for i from 0
          for thing across queue
          do (with-simple-restart (continue "Continue processing messages, skipping ~a" thing)
               (pass pipeline thing))
             (setf (aref queue i) 0)))
  (setf (fill-pointer queue) 0))

(defmethod controller-loop ((controller controller))
  (let* ((lock (queue-lock controller))
         (condition (queue-condition controller))
         (pipeline (pipeline controller)))
    (bt:acquire-lock lock)
    (unwind-protect
         (loop do (let ((queue (queue controller)))
                    (rotatef (queue controller) (queue-back controller))
                    (bt:release-lock lock)
                    (process-message-batch queue pipeline))
                  (bt:acquire-lock lock)
                  (loop while (= 0 (length (queue controller)))
                        do (bt:condition-wait condition lock :timeout 1))
               while (thread-continue controller))
      (setf (thread controller) NIL)
      (ignore-errors (bt:release-lock lock)))))

(defmethod pass ((controller controller) message)
  (let ((thread (thread controller)))
    (cond ((and (not *process-locally*)
                thread (bt:thread-alive-p thread)
                (not (eql (bt:current-thread) thread)))
           (with-controller-lock (controller)
             (vector-push-extend message (queue controller)))
           (bt:condition-notify (queue-condition controller)))
          (T
           (pass (pipeline controller) message))))
  NIL)
