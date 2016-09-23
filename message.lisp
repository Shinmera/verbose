#|
 This file is a part of Verbose
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defvar *levels* NIL)
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

(defmethod format-message ((null null) (func function))
  (princ-to-string (funcall func)))

(defmethod format-message ((null null) (message message))
  (with-output-to-string (stream)
    (format-message stream message)))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type T)
    (format-message stream message)))

(defun log-message (level categories content &optional (class *default-message-class*) &rest initargs)
  (unless (listp categories)
    (setf categories (list categories)))
  (unless (find level *levels* :key #'cdr)
    (error "Level must be one of ~a" (mapcar #'cdr *levels*)))
  (unless (every #'keywordp categories) 
    (error "Categories must be keywords."))
  (log-object (apply #'make-instance class :level level :categories categories :content content initargs)))

(defmethod log (level categories (datum string) &rest args)
  (log-message level categories (apply #'format NIL datum args)))

(defmethod log (level categories (datum symbol) &rest args)
  (log level categories (apply (if (subtypep datum 'condition)
                                   #'make-condition
                                   #'make-instance)
                               datum args)))

(defmethod log (level categories (datum function) &rest args)
  (log-message level categories (lambda () (apply datum args))))

(defmethod log (level categories datum &rest args)
  (declare (ignore args))
  (log-message level categories datum))

(defun add-level (priority level)
  (if (find level *levels* :key #'cdr)
      (setf (car (find level *levels* :key #'cdr)) priority)
      (push (cons priority level) *levels*))
  (setf *levels* (sort *levels* #'< :key #'car)))

(defmacro define-level (priority level &optional (name (intern (string level) :v)))
  (check-type priority integer)
  (check-type name symbol)
  `(progn
     (add-level ,priority ',level)

     (defun ,name (categories datum &rest args)
       (dissect:with-capped-stack ()
         (apply #'log ',level categories datum args)))

     (define-compiler-macro ,name (categories datum &rest args)
       `(dissect:with-capped-stack ()
          (log ',',level ,categories ,datum ,@args)))

     (export ',name)))

(define-level -10 :trace)
(define-level  -5 :debug)
(define-level   0 :info)
(define-level   5 :warn)
(define-level  10 :error)
(define-level  15 :severe)
(define-level  20 :fatal)
