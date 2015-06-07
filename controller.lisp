#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defvar *global-controller* NIL "Global variable holding the current verbose controller instance and pipeline.")
(defvar *muffled-categories* NIL "Which categories of messages that are handed to PASS to muffle (not put onto the pipeline).")

(defclass controller (pipeline)
  ((thread :initform NIL :accessor controller-thread)
   (shares :initform (make-hash-table) :accessor shares)
   (message-condition :initform (bt:make-condition-variable :name "MESSAGE-CONDITION") :reader message-condition)
   (message-pipe :initform (make-array '(10) :adjustable T :fill-pointer 0) :accessor message-pipe)
   (message-lock :initform (bt:make-lock "MESSAGE-LOCK") :reader message-lock))
  (:documentation "Main controller class that holds the logging pipeline, thread and shares."))

(defmethod initialize-instance :after ((controller controller) &rest rest)
  (declare (ignore rest))
  (set-standard-special-values controller)
  #+:thread-support
  (setf (controller-thread controller)
        (bt:make-thread #'controller-loop
                        :name "CONTROLLER MESSAGE LOOP"
                        :initial-bindings `((*global-controller* . ,controller)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-controller-lock ((&optional (controller '*global-controller*)) &body forms)
    "Wraps the body into an environment with the thread lock for the controller acquired so it is safe to modify the pipeline or controller slots."
    `(bt:with-lock-held ((message-lock ,controller))
       ,@forms)))

(defun share (name &optional (controller *global-controller*))
  "Return the value of a shared special variable.

Shared bindings are special variable bindings that are active during the message
passing in the controller's logging thread. This allows you to influence various
behaviours of the pipeline, such as where and how the messages are printed.

This is thread-safe."
  (with-controller-lock (controller)
    (gethash name (shares controller))))

(defun (setf share) (value name &optional (controller *global-controller*))
  "Set the value of a shared special variable.

This is thread-safe.

See SHARE."
  (with-controller-lock (controller)
    (setf (gethash name (shares controller)) value)))

(defun shared-bindings (controller)
  "Returns a cons of two lists, the first list containing the binding names and the second the values.
For use with PROGV.

See WITH-SHARES."
  (loop for k being the hash-keys of (shares controller)
        for v being the hash-values of (shares controller)
        collect k into keys
        collect v into vals
        finally (return (cons keys vals))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-shares ((&optional (controller '*global-controller*)) &body body)
    "Binds all shared special variables of CONTROLLER within BODY.

See SHARE."
    (let ((shares (gensym "SHARES")))
      `(let ((,shares (shared-bindings ,controller)))
         (progv (car ,shares) (cdr ,shares)
           ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro copy-bindings (controller &body variables)
    "Copy all the values of VARIABLEs over to shared bindings in the CONTROLLER.

See (SETF SHARE)."
    (let ((contr (gensym "CONTROLLER")))
      `(let ((,contr ,controller))
         ,@(loop for var in variables
                 collect `(setf (share ',var ,contr) ,var))))))

(defun set-standard-special-values (controller)
  "Set all standard special values from CL as shared bindings.

See COPY-BINDINGS."
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
    (loop do
      (with-simple-restart (skip "Skip processing the message.")
        (let ((queue (message-pipe controller)))
          (setf (message-pipe controller) (make-array '(10) :adjustable T :fill-pointer 0))
          (bt:release-lock lock)
          (with-shares (controller)
            (loop for message across queue
                  do (pass pipeline message)))))
      (bt:acquire-lock lock)
      (when (= 0 (length (message-pipe controller)))
        (bt:condition-wait condition lock)))))

(defmethod pass ((controller controller) message)
  "Pass a raw message into the controller pipeline. 

Note that this is not instant as the message passing is done in a separate thread.
This function returns as soon as the message is added to the queue, which should be
near-instant.

In the case that no live thread is available on the controller, either due to a lack
of thread support entirely, or some other factor, the message is passed along to the
pipeline directly. This will mean it is processed immediately, but it also means it
will hold up the current thread.

Also note that, depending on *MUFFLED-CATEGORIES*, some messages may not actually
be put onto the pipeline at all."
  (unless (or (find T *muffled-categories*)
              (loop for category in *muffled-categories*
                    thereis (find category (message-categories message))))
    (cond ((and (controller-thread controller)
                (bt:thread-alive-p (controller-thread controller)))
           (with-controller-lock (controller)
             (vector-push-extend message (message-pipe controller)))
           (bt:condition-notify (message-condition controller)))
          ;; If for some reason the thread is not around (maybe it broke, maybe
          ;; there's no threads whatsoever) just pass it along directly.
          (T
           (with-shares (controller)
             (pass (pipeline controller) message)))))
  NIL)

(flet ((copy-function (from to)
         (setf (fdefinition to) (fdefinition from)
               (documentation to 'function) (documentation from 'function))))
  (copy-function 'share 'shared-instance)
  (copy-function '(setf share) '(setf shared-instance)))


(defmacro with-muffled-logging ((&optional (category T) &rest more-categories) &body body)
  "Muffles all messages of CATEGORY within BODY.
This means that all logging statements that fit CATEGORY within the BODY
are not actually ever handed to the pipeline. If CATEGORY is T, all
messages are muffled."
  `(let ((*muffled-categories* (list* ,category ,@more-categories *muffled-categories*)))
     ,@body))
