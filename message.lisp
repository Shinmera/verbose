#|
 This file is a part of Verbose
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *levels* (list :TRACE :DEBUG :INFO :WARN :ERROR :SEVERE :FATAL)))

(defvar *timestamp-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
(defvar *default-message-class* 'message)

(defun log-object (object)
  (when *global-controller*
    (pass *global-controller* object)))

(defclass message ()
  ((timestamp :initarg :timestamp :accessor timestamp)
   (thread :initarg :thread :accessor thread)
   (level :initarg :level :accessor level)
   (categories :initarg :categories :accessor categories)
   (content :initarg :content :accessor content))
  (:default-initargs
   :timestamp (local-time:now)
   :thread (bt:current-thread)
   :level :info
   :categories ()
   :content NIL))

(defmethod format-message ((stream stream) (message message))
  (format stream "~a [~5,a] ~{<~a>~}: ~a"
          (local-time:format-timestring NIL (timestamp message) :format *timestamp-format*)
          (level message)
          (categories message)
          (format-message NIL (content message))))

(defmethod format-message ((null null) message)
  (princ-to-string message))

(defmethod format-message ((null null) (message message))
  (with-output-to-string (stream)
    (format-message stream message)))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type T)
    (format-message stream message)))

(defun log-message (level categories content &optional (class *default-message-class*) &rest initargs)
  (unless (listp categories)
    (setf categories (list categories)))
  (assert (find level *levels*) (level)
          "Level must be one of (:FATAL :SEVERE :ERROR :WARN :INFO :DEBUG :TRACE).")
  (assert (every #'keywordp categories) (categories)
          "Categories must be a list of keywords.")
  (log-object (apply #'make-instance class :level level :categories categories :content content initargs)))

(defmethod log (level categories (datum string) &rest args)
  (log-message level categories (apply #'format NIL datum args)))

(defmethod log (level categories (datum symbol) &rest args)
  (log level categories (apply (if (typep class 'condition-class)
                                   #'make-condition
                                   #'make-instance)
                               datum args)))

(defmethod log (level categories (datum function) &rest args)
  (log-message level categories (lambda () (apply datum args))))

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
