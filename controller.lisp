#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defvar *message-pipe* (make-array '(50) :adjustable T :fill-pointer 0))
(defvar *message-lock* (make-lock "Message Lock"))

(defclass controller (source)
  ((thread :accessor controller-thread))
  (:documentation "Main controller class that holds the logging construct."))

(defmethod initialize-instance :after ((controller controller) &rest rest)
  (declare (ignore rest))
  (setf (controller-thread controller)
        (make-thread #'controller-loop
                     :initial-bindings `((*standard-output* . ,*standard-output*)
                                         (*error-output* . ,*error-output*)))))

(defun controller-loop ()
  (loop do
       (with-lock-held (*message-lock*)
         (let ((length (length *message-pipe*)))
           (loop for i below length
              for message = (vector-pop *message-pipe*)
              do (pass (next *global-controller*) message))))
       (thread-yield)))

(defvar *global-controller* (make-instance 'controller))
