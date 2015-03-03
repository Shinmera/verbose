#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defvar *global-controller* NIL "Global variable holding the current verbose controller instance and pipeline..")
(defvar *muffled-categories* NIL "Which categories of messages that are handed to PASS to muffle (not put onto the pipeline).")

(defclass controller (pipeline)
  ((thread :accessor controller-thread)
   (shares :initform (make-hash-table) :accessor shares)
   (message-condition :initform (make-condition-variable :name "MESSAGE-CONDITION") :reader message-condition)
   (message-pipe :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor message-pipe)
   (message-lock :initform (make-lock "MESSAGE-LOCK") :reader message-lock))
  (:documentation "Main controller class that holds the logging pipeline, thread and shares."))

(defmethod initialize-instance :after ((controller controller) &rest rest)
  (declare (ignore rest))
  (setf (gethash '*standard-output* (shares controller)) *standard-output*)
  (setf (gethash '*error-output* (shares controller)) *error-output*)
  (setf (controller-thread controller)
        (make-thread #'controller-loop
                     :name "CONTROLLER MESSAGE LOOP"
                     :initial-bindings `((*global-controller* . ,controller)))))

(defmacro with-controller-lock ((&optional (controller '*global-controller*)) &body forms)
  "Wraps the body into an environment with the thread lock for the controller acquired so it is safe to modify the pipeline or controller slots."
  `(with-lock-held ((message-lock ,controller))
     ,@forms))

(defun controller-loop ()
  (let* ((controller *global-controller*)
         (lock (message-lock controller))
         (condition (message-condition controller))
         (pipeline (pipeline controller)))
    (acquire-lock lock)
    (loop do
      (with-simple-restart (skip "Skip processing the message.")
        (let ((queue (message-pipe controller))
              (*standard-output* (gethash '*standard-output* (shares controller)))
              (*error-output* (gethash '*error-output* (shares controller))))
          (setf (message-pipe controller) (make-array '(10) :adjustable T :fill-pointer 0))
          (release-lock lock)
          (loop for message across queue
                do (pass pipeline message))))
      (acquire-lock lock)
      (when (= 0 (length (message-pipe controller)))
        (condition-wait condition lock)))))

(defmethod pass ((controller controller) message)
  "Pass a raw message into the controller pipeline. 

Note that this is not instant as the message passing is done in a separate thread.
This function returns as soon as the message is added to the queue, which should be
near-instant.

Also note that, depending on *MUFFLED-CATEGORIES*, some messages may not actually
be put onto the pipeline at all."
  (unless (or (find T *muffled-categories*)
              (loop for category in *muffled-categories*
                    thereis (find category (message-categories message))))
    (with-controller-lock (controller)
      (vector-push-extend message (message-pipe controller)))
    (condition-notify (message-condition controller)))
  NIL)

(defun shared-instance (symbol)
  "Return a shared instance identified by the symbol."
  (with-controller-lock ()
    (gethash symbol (shares *global-controller*))))

(defgeneric (setf shared-instance) (val symbol)
  (:documentation "Set the shared instance identified by the symbol.")
  (:method (val symbol)
    (with-controller-lock ()
      (setf (gethash symbol (shares *global-controller*)) val))))

(defmacro with-muffled-logging ((&optional (category T) &rest more-categories) &body body)
  "Muffles all messages of CATEGORY within BODY.
This means that all logging statements that fit CATEGORY within the BODY
are not actually ever handed to the pipeline. If CATEGORY is T, all
messages are muffled."
  `(let ((*muffled-categories* (list* ,category ,@more-categories *muffled-categories*)))
     ,@body))
