#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defvar *global-controller* NIL)
(defvar *controller-standard-output* *standard-output*)
(defvar *controller-error-output* *error-output*)

(defclass controller (pipeline)
  ((source :initarg :source :initform () :accessor source)
   (thread :accessor controller-thread)
   (message-condition :initform (make-condition-variable :name "MESSAGE-CONDITION") :reader message-condition)
   (message-pipe :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor message-pipe)
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
        (make-thread #'controller-loop
                     :name "CONTROLLER MESSAGE LOOP"
                     :initial-bindings `((*controller-standard-output* . ,*controller-standard-output*)
                                         (*controller-error-output* . ,*controller-error-output*)
                                         (*standard-output* . ,*controller-standard-output*)
                                         (*error-output* . ,*controller-error-output*)
                                         (*global-controller* . ,controller)))))

(defun controller-loop ()
  (let* ((controller *global-controller*)
         (lock (message-lock controller))
         (condition (message-condition controller))
         (source (source controller)))
    (acquire-lock lock)
    (loop do
      (with-simple-restart (skip "Skip processing the message.")
        (let ((queue (message-pipe controller)))
          (setf (message-pipe controller) (make-array '(10) :adjustable T :fill-pointer 0))
          (release-lock lock)
          (loop for message across queue
                do (pass source message))))
      (acquire-lock lock)
      (condition-wait condition lock))))

(defmethod pass ((controller controller) message)
  (with-lock-held ((message-lock controller))
    (vector-push-extend message (message-pipe controller)))
  (condition-notify (message-condition controller))
  NIL)

(defun remove-global-controller ()
  (when (thread-alive-p (controller-thread *global-controller*))
    (destroy-thread (controller-thread *global-controller*)))
  (setf *global-controller* NIL))
