#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defvar *global-controller* NIL)
(defvar *muffled-categories* NIL)
(defvar *process-locally* NIL)

(defstruct sync-request
  (condition (bt:make-condition-variable))
  (lock (bt:make-lock)))

(defclass controller (pipeline)
  ((thread :initform NIL :accessor controller-thread)
   (thread-continue :initform NIL :accessor controller-thread-continue)
   (shares :initform (make-hash-table) :accessor shares)
   (message-condition :initform (bt:make-condition-variable :name "MESSAGE-CONDITION") :reader message-condition)
   (message-pipe :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor message-pipe)
   (message-lock :initform (bt:make-lock "MESSAGE-LOCK") :reader message-lock)))

(defmethod initialize-instance :after ((controller controller) &key)
  (set-standard-special-values controller)
  (start-controller controller))

(defmethod print-object ((controller controller) stream)
  (print-unreadable-object (controller stream :type T)
    (format stream "~@[:threaded~*~]~@[ :running~*~] :queue-size ~d"
            (controller-thread controller) (controller-thread-continue controller) (length (message-pipe controller)))))

(defun start-controller (&optional (controller *global-controller*))
  (setf (controller-thread-continue controller) T)
  #+:thread-support
  (setf (controller-thread controller)
        (bt:make-thread #'controller-loop
                        :name "verbose-controller-thread"
                        :initial-bindings `((*global-controller* . ,controller)))))

(defun stop-controller (&optional (controller *global-controller*))
  (setf (controller-thread-continue controller) NIL)
  #+:thread-support
  (loop with thread = (controller-thread controller)
        for i from 0
        while (and thread (bt:thread-alive-p thread))
        do (bt:condition-notify (message-condition controller))
           (sleep 0.1)
           (when (< 5 i)
             (bt:destroy-thread thread)
             (return)))
  controller)

(defmacro with-controller-lock ((&optional (controller '*global-controller*)) &body forms)
  `(bt:with-lock-held ((message-lock ,controller))
     ,@forms))

(defun share (name &optional (controller *global-controller*))
  (with-controller-lock (controller)
    (gethash name (shares controller))))

(defun (setf share) (value name &optional (controller *global-controller*))
  (with-controller-lock (controller)
    (setf (gethash name (shares controller)) value)))

(defun shared-instance (name &optional (controller *global-controller*))
  (share name controller))

(defun (setf shared-instance) (value name &optional (controller *global-controller*))
  (setf (share name controller) value))

(defun shared-bindings (controller)
  (loop for k being the hash-keys of (shares controller)
        for v being the hash-values of (shares controller)
        collect k into keys
        collect v into vals
        finally (return (cons keys vals))))

(defmacro with-shares ((&optional (controller '*global-controller*)) &body body)
  (let ((shares (gensym "SHARES")))
    `(let ((,shares (shared-bindings ,controller)))
       (progv (car ,shares) (cdr ,shares)
         ,@body))))

(defmacro copy-bindings (controller &body variables)
  (let ((contr (gensym "CONTROLLER")))
    `(let ((,contr ,controller))
       ,@(loop for var in variables
               collect `(setf (share ',var ,contr) ,var)))))

(defun set-standard-special-values (controller)
  (copy-bindings controller
    *break-on-signals*
    *compile-file-pathname*
    *compile-file-truename*
    *compile-print*
    *compile-verbose*
    *debug-io*
    *debugger-hook*
    *default-pathname-defaults*
    *error-output*
    *features*
    *gensym-counter*
    *load-pathname*
    *load-print* *load-truename* *load-verbose*
    *macroexpand-hook*
    *modules*
    *package*
    *print-array*
    *print-base*
    *print-case*
    *print-circle*
    *print-escape*
    *print-gensym*
    *print-length*
    *print-level*
    *print-lines*
    *print-miser-width*
    *print-pprint-dispatch*
    *print-pretty*
    *print-radix*
    *print-readably*
    *print-right-margin*
    *query-io*
    *random-state*
    *read-base*
    *read-default-float-format*
    *read-eval*
    *read-suppress*
    *readtable*
    *standard-input*
    *standard-output*
    *terminal-io*
    *trace-output*))

(defun controller-loop ()
  (let* ((controller *global-controller*)
         (lock (message-lock controller))
         (condition (message-condition controller))
         (pipeline (pipeline controller)))
    (bt:acquire-lock lock)
    (unwind-protect
         (loop do (with-simple-restart (skip "Skip processing the message batch.")
                    (let ((queue (message-pipe controller)))
                      (setf (message-pipe controller) (make-array '(10) :adjustable T :fill-pointer 0))
                      (bt:release-lock lock)
                      (with-shares (controller)
                        (loop for thing across queue
                              do (with-simple-restart (continue "Continue processing messages, skipping ~a" thing)
                                   (etypecase thing
                                     (message (pass pipeline thing))
                                     (sync-request
                                      ;; Make sure it's actually waiting on the condition.
                                      (bt:with-lock-held ((sync-request-lock thing)))
                                      (bt:condition-notify (sync-request-condition thing)))))))))
                  (bt:acquire-lock lock)
                  (when (= 0 (length (message-pipe controller)))
                    (bt:condition-wait condition lock))
               while (controller-thread-continue controller))
      (setf (controller-thread controller) NIL)
      (ignore-errors (bt:release-lock lock)))))

(defmethod pass ((controller controller) message)
  (let ((filtered-categories (loop for category in (message-categories message)
                                   unless (loop for filter in *muffled-categories*
                                                thereis (matching-tree-category filter category))
                                   collect category)))
    (setf (message-categories message) filtered-categories)
    (when (and filtered-categories
               (not (find T *muffled-categories*)))
      (cond ((and (not *process-locally*)
                  (controller-thread controller)
                  (bt:thread-alive-p (controller-thread controller)))
             (with-controller-lock (controller)
               (vector-push-extend message (message-pipe controller)))
             (bt:condition-notify (message-condition controller)))
            ;; If for some reason the thread is not around (maybe it broke, maybe
            ;; there's no threads whatsoever) just pass it along directly.
            (T
             (with-shares (controller)
               (pass (pipeline controller) message))))))
  NIL)

(defmacro with-muffled-logging ((&optional (category T) &rest more-categories) &body body)
  `(let ((*muffled-categories* (list* ,category ,@more-categories *muffled-categories*)))
     ,@body))

(defun sync (&optional (controller *global-controller*))
  (when (and (controller-thread controller)
             (bt:thread-alive-p (controller-thread controller)))
    (let ((sync (make-sync-request)))
      (bt:with-lock-held ((sync-request-lock sync))
        (with-controller-lock (controller)
          (vector-push-extend sync (message-pipe controller)))
        (bt:condition-notify (message-condition controller))
        (bt:condition-wait (sync-request-condition sync) (sync-request-lock sync))))))
