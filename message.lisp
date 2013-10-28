#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defun log-object (object)
  (pass *global-controller* object))

(defun get-current-thread ()
  #+sbcl sb-thread:*current-thread*)

(defclass message ()
  ((time :initarg :time :initform (local-time:now) :accessor message-time)
   (thread :initarg :thread :initform (get-current-thread) :accessor message-thread)
   (level :initarg :level :initform :INFO :accessor message-level)
   (category :initarg :category :initform :GENERAL :accessor message-category)
   (content :initarg :content :initform NIL :accessor message-content))
  (:documentation "Message object to be passed around the system."))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream)
    (format stream "~a | [~5,a] <~a>: ~a" 
            (local-time:format-timestring NIL (message-time message))
            (message-level message)
            (message-category message)
            (message-content message))))

(defun log-message (level category content)
  (log-object (make-instance 'message :level level :category category :content content)))

(defun log (level category format-string &rest format-args)
  (log-message level category (apply #'format NIL format-string format-args)))

