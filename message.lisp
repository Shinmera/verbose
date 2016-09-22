#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *levels* (list :TRACE :DEBUG :INFO :WARN :ERROR :SEVERE :FATAL)))

(defvar *verbose-conditions* T)
(defvar *timestamp-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defun log-object (object)
  (when *global-controller*
    (pass *global-controller* object)))

(defclass message ()
  ((time :initarg :time :accessor message-time)
   (thread :initarg :thread :accessor message-thread)
   (level :initarg :level :accessor message-level)
   (categories :initarg :categories :accessor message-categories)
   (content :initarg :content :accessor message-content))
  (:default-initargs
   :time (local-time:now)
   :thread (bt:current-thread)
   :level :info
   :categories '(:general)
   :content NIL))

(defmethod format-message ((stream stream) (message message))
  (format stream "~&~a [~5,a] ~{<~a>~}: ~a~%"
          (local-time:format-timestring NIL (message-time message) :format *timestamp-format*)
          (message-level message)
          (message-categories message)
          (format-message NIL (message-content message)))
  (force-output stream))

(defmethod format-message ((null null) (message message))
  (with-output-to-string (stream)
    (format-message stream message)))

(defmethod format-message ((null null) message)
  message)

(defmethod format-message ((null null) (message function))
  (funcall message))

(defclass condition-message (message)
  ((condition :initarg :condition :accessor message-condition))
  (:default-initargs
   :condition (cl:error "CONDITION required.")))

;; Backwards compat.
(defmethod message-category ((message message))
  (first (message-categories message)))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream)
    (format stream "~a [~5,a] ~{<~a>~}: ~a"
            (local-time:format-timestring NIL (message-time message) :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
            (message-level message)
            (message-categories message)
            (message-content message)))
  message)

(defmethod message-visible ((message message) (level symbol))
  (<= (position level *levels*) (position (message-level message) *levels*)))

(defun log-message (level categories content &optional (class 'message) &rest initargs)
  (unless (listp categories)
    (setf categories (list categories)))
  (assert (find level *levels*) (level)
          "Level must be one of (:FATAL :SEVERE :ERROR :WARN :INFO :DEBUG :TRACE).")
  (assert (every #'keywordp categories) (categories)
          "Categories must be a list of keywords.")
  (log-object (apply #'make-instance class :level level :categories categories :content content initargs)))

(defun log-condition (level categories condition)
  (let ((content (if *verbose-conditions*
                     (with-output-to-string (stream)
                       (dissect:present condition stream))
                     (princ-to-string condition))))
    (log-message level categories content 'condition-message :condition condition)))

(defmethod log (level categories (datum string) &rest args)
  (log-message level categories (apply #'format NIL datum args)))

(defmethod log (level categories (datum symbol) &rest args)
  (log level categories (apply (if (subtypep datum 'condition)
                                   #'make-condition
                                   #'make-instance)
                               datum args)))

(defmethod log (level categories (datum function) &rest args)
  (log-message level categories (lambda () (apply datum args))))

(defmethod log (level categories (datum condition) &rest args)
  (declare (ignore args))
  (log-condition level categories datum))

(defmethod log (level categories datum &rest args)
  (declare (ignore args))
  (log-message level categories datum))

(defmacro define-level (level)
  (let ((func (intern (symbol-name level) "VERBOSE")))
    `(progn
       (defun ,func (categories format-string &rest format-args)
         (dissect:with-capped-stack ()
           (apply #'log ',level categories format-string format-args)))

       (define-compiler-macro ,func (categories format-string &rest format-args)
         `(funcall (load-time-value
                    (progn
                      (unless (find-package :verbose)
                        #+quicklisp (ql:quickload :verbose :silent T)
                        #-quicklisp (asdf:load-system :verbose))
                      (find-symbol (string :log) :verbose)))
                   ',,level ,categories ,format-string ,@format-args)))))

(macrolet ((define-all ()
             `(progn ,@(loop for level in *levels*
                             collect `(define-level ,level)))))
  (define-all))
