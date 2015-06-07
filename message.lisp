#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

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

(defgeneric log (level categories datum &rest datum-args)
  (:documentation "Logs a new message under given level and categories, using the datum and datum-args to construct the content.

The following datum classes are recognised by default:
STRING     -- Construct the content through FORMAT with datum and datum-args.
SYMBOL     -- If datum names a condition class, MAKE-CONDITION is called with
              the datum and datum-args. Otherwise MAKE-INSTANCE is used.
FUNCTION   -- Use a (lambda () (apply datum datum-args)) as the content.
              By default this means that the given function will be called
              within the controller thread. See FORMAT-MESSAGE.
T          -- Discard the datum-args and use datum directly as content.")
  (:method (level categories (datum string) &rest args)
    (log-message level categories (apply #'format NIL datum args)))
  (:method (level categories (datum symbol) &rest args)
    (log-message level categories (apply (if (subtypep datum 'condition)
                                             #'make-condition
                                             #'make-instance)
                                         datum args)))
  (:method (level categories (datum function) &rest args)
    (log-message level categories (lambda () (apply datum args))))
  (:method (level categories datum &rest args)
    (declare (ignore args))
    (log-message level categories datum)))

(defmacro define-level (level)
  "Define a new shorthand function for logging under a specific level."
  `(defun ,(intern (symbol-name level) "VERBOSE") (category datum &rest datum-args)
     (apply #'log ,(intern (symbol-name level) "KEYWORD") category datum datum-args)))

(macrolet ((define-all ()
             `(progn ,@(loop for level in *levels*
                             collect `(define-level ,level)))))
  (define-all))
