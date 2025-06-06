(in-package #:org.shirakumo.verbose)

(defvar *levels* NIL)
(defvar *default-message-class* 'message)
#-sbcl (defvar *pool* NIL)
#+sbcl (sb-ext:defglobal *pool* NIL)

(declaim (inline log-object))
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
   :timestamp (get-universal-time)
   :thread (bt:current-thread)
   :level :info
   :categories ()
   :content NIL))

(defmethod initialize-instance :before ((message message) &key level categories)
  (unless (find level *levels* :key #'cdr)
    (cl:error "Level must be one of ~a" (mapcar #'cdr *levels*)))
  (unless (every #'keywordp categories) 
    (cl:error "Categories must be keywords.")))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type T)
    (format-message stream message)))

(defmethod format-message ((null null) thing)
  (with-output-to-string (stream)
    (format-message stream thing)))

(defmethod format-message ((vector string) thing)
  (with-output-to-string (stream vector)
    (format-message stream thing)))

(defmethod format-message ((stream stream) (message message))
  (format-time :timestamp (timestamp message) :stream stream)
  (format stream " [~5,a] ~{<~a>~}: "
          (level message)
          (categories message))
  (format-message stream (content message)))

(defmethod format-message ((stream stream) thing)
  (princ thing stream))

(defmethod format-message (target (func function))
  (format-message target (funcall func)))

(setf *pool* (make-pool (lambda () (allocate-instance (find-class 'message)))))

(defun maybe-release-message (message)
  (when (and *pool* (eq (type-of message) 'message))
    (release-instance *pool* message)))

(defun maybe-draw-message (level categories content class &rest initargs)
  (if (and *pool* (eq class 'message))
      (let ((instance (draw-instance *pool*)))
        (setf (timestamp instance) (get-universal-time))
        (setf (thread instance) (bt:current-thread))
        (setf (level instance) level)
        (setf (categories instance) categories)
        (setf (content instance) content)
        instance)
      (apply #'make-instance class :level level :categories categories :content content initargs)))

(defun log-message (level categories content &optional (class *default-message-class*) &rest initargs)
  (unless (listp categories)
    (setf categories (list categories)))
  (log-object (apply #'maybe-draw-message level categories content class initargs)))

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

(defmacro define-level (priority level &optional (name (intern (string level) '#:org.shirakumo.verbose)))
  (check-type priority integer)
  (check-type name symbol)
  `(progn
     (add-level ,priority ',level)

     (defun ,name (categories datum &rest args)
       ,(format NIL "Log to the ~a level." level)
       (dissect:with-capped-stack ()
         (apply #'log ',level categories datum args)))

     (define-compiler-macro ,name (categories datum &rest args)
       `(dissect:with-capped-stack ()
          (log ',',level ,categories ,datum ,@args)))

     ,(when (eql (symbol-package name) (find-package '#:org.shirakumo.verbose))
        `(export ',name))
     ',name))

(define-level -10 :trace)
(define-level  -5 :debug)
(define-level   0 :info)
(define-level   5 :warn)
(define-level  10 :error)
(define-level  15 :severe)
(define-level  20 :fatal)
