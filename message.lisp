#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defvar *levels* (list :TRACE :DEBUG :INFO :WARN :ERROR :SEVERE :FATAL)
  "List of accepted logging level names. The position in the list defines its level, ranking from finest to roughest.")

(defun log-object (object)
  "Shorthand for (pass *global-controller* object)"
  (pass *global-controller* object))

(defclass message ()
  ((time :initarg :time :initform (local-time:now) :accessor message-time)
   (thread :initarg :thread :initform (bordeaux-threads:current-thread) :accessor message-thread)
   (level :initarg :level :initform :INFO :accessor message-level)
   (category :initarg :category :initform :GENERAL :accessor message-category)
   (content :initarg :content :initform NIL :accessor message-content))
  (:documentation "Message object to be passed around the system."))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream)
    (format stream "~a [~5,a] <~a>: ~a"
            (local-time:format-timestring NIL (message-time message) :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
            (message-level message)
            (message-category message)
            (message-content message)))
  message)

(defgeneric message-visible (message level)
  (:documentation "Returns T if the message is visible under the given level.")
  (:method ((message message) (level symbol))
    (<= (position level *levels*) (position (message-level message) *levels*))))

(defun log-message (level category content)
  "Logs a message with the given parameters."
  (assert (find level *levels*) (level)
          "Level must be one of (:FATAL :SEVERE :ERROR :WARN :INFO :DEBUG :TRACE).")
  (assert (keywordp category) (category)
          "Category must be a keyword.")
  (log-object (make-instance 'message :level level :category category :content content)))

(defun log (level category format-string &rest format-args)
  "Logs a new message under given level and category, using the format-string and arguments as message text."
  (log-message level category (apply #'format NIL format-string format-args)))

(defmacro define-level (level)
  "Define a new shorthand function for logging under a specific level."
  `(defun ,(intern (symbol-name level) "VERBOSE") (category format-string &rest format-args)
     (apply #'log ,(intern (symbol-name level) "KEYWORD") category format-string format-args)))

(eval `(progn ,@(loop for level in *levels*
                      collect `(define-level ,level))))
