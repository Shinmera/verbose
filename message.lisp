#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *levels* (list :TRACE :DEBUG :INFO :WARN :ERROR :SEVERE :FATAL)
    "List of accepted logging level names. The position in the list defines its level, ranking from finest to roughest."))

(defun log-object (object)
  "Shorthand for (pass *global-controller* object)"
  (pass *global-controller* object))

(defclass message ()
  ((time :initarg :time :initform (local-time:now) :accessor message-time)
   (thread :initarg :thread :initform (bordeaux-threads:current-thread) :accessor message-thread)
   (level :initarg :level :initform :INFO :accessor message-level)
   (categories :initarg :categories :initform '(:GENERAL) :accessor message-categories)
   (content :initarg :content :initform NIL :accessor message-content))
  (:documentation "Message object to be passed around the system."))

;; Backwards compat.
(defgeneric message-category (message)
  (:method ((message message))
    (first (message-categories message))))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream)
    (format stream "~a [~5,a] ~{<~a>~}: ~a"
            (local-time:format-timestring NIL (message-time message) :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
            (message-level message)
            (message-categories message)
            (message-content message)))
  message)

(defgeneric message-visible (message level)
  (:documentation "Returns T if the message is visible under the given level.")
  (:method ((message message) (level symbol))
    (<= (position level *levels*) (position (message-level message) *levels*))))

(defun log-message (level categories content)
  "Logs a message with the given parameters."
  (unless (listp categories)
    (setf categories (list categories)))
  (assert (find level *levels*) (level)
          "Level must be one of (:FATAL :SEVERE :ERROR :WARN :INFO :DEBUG :TRACE).")
  (assert (every #'keywordp categories) (categories)
          "Categories must be a list of keywords.")
  (log-object (make-instance 'message :level level :categories categories :content content)))

(defun log (level categories format-string &rest format-args)
  "Logs a new message under given level and categories, using the format-string and arguments as message text."
  (log-message level categories (apply #'format NIL format-string format-args)))

(defmacro define-level (level)
  "Define a new shorthand function for logging under a specific level."
  `(defun ,(intern (symbol-name level) "VERBOSE") (category format-string &rest format-args)
     (apply #'log ,(intern (symbol-name level) "KEYWORD") category format-string format-args)))

(macrolet ((define-all ()
             `(progn ,@(loop for level in *levels*
                             collect `(define-level ,level)))))
  (define-all))
