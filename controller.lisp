#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defclass controller (pipeline)
  ((source :initarg :source :initform () :accessor source)
   (thread :accessor controller-thread)
   (message-condition :initform (make-condition-variable :name "MESSAGE-CONDITION") :reader message-condition)
   (message-pipe :initform (make-array '(50) :adjustable T :fill-pointer 0) :reader message-pipe)
   (message-lock :initform (make-lock "MESSAGE-LOCK") :reader message-lock))
  (:documentation "Main controller class that holds the logging construct."))

(defmethod initialize-instance :after ((controller controller) &rest rest)
  (declare (ignore rest))
  
  (unless (source controller)
    (let ((source (add-pipe controller 'source :name "SOURCE"))
          (faucet (add-pipe controller 'print-faucet :name "PRINTER")))
      (connect-next source faucet)
      (setf (source controller) source)))

  (setf (controller-thread controller)
        (make-thread #'controller-loop :name "CONTROLLER MESSAGE LOOP"
                     :initial-bindings `((*standard-output* . ,*standard-output*)
                                         (*error-output* . ,*error-output*)
                                         (*global-controller* . ,controller)))))

(defun controller-loop ()
  (let* ((controller *global-controller*)
         (lock (message-lock controller))
         (condition (message-condition controller))
         (message-pipe (message-pipe controller))
         (source (source controller)))
    (acquire-lock lock)
    (loop do
         (let ((length (length message-pipe)))
           (loop for i from 0 below length
              for message = (vector-pop message-pipe)
              do (pass source message)))
         (condition-wait condition lock))))

(defmethod pass ((controller controller) message)
  (with-lock-held ((message-lock controller))
    (vector-push-extend message (message-pipe controller)))
  (condition-notify (message-condition controller))
  NIL)
