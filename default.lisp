#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defvar *repl-level* :INFO)

(defun make-standard-global-controller ()
  (let ((pipeline (make-instance 'controller))
        (pipe (make-pipe)))
    (insert (make-instance 'predicate-filter :predicate #'(lambda (message) (eql (message-level message) *repl-level*))) pipe)
    (insert (make-instance 'category-tree-filter) pipe)
    (insert (make-instance 'repl-faucet) pipe)
    (add-segment pipeline pipe)
    pipeline))

(defun remove-global-controller ()
  (when (thread-alive-p (controller-thread *global-controller*))
    (destroy-thread (controller-thread *global-controller*)))
  (setf *global-controller* NIL))

(defun restart-global-controller ()
  (remove-global-controller)
  (setf *global-controller* (make-standard-global-controller)))

(defun set-repl-level (level)
  (with-lock-held ((message-lock *global-controller*))
    (setf *repl-level* level)))

(defun set-repl-categories (categories)
  (with-lock-held ((message-lock *global-controller*))
    (setf (categories (find-place *global-controller* 'repl-category-filter))
          categories)))

(defun add-repl-category (category)
  (with-lock-held ((message-lock *global-controller*))
    (let ((categories (categories (find-place *global-controller* 'repl-category-filter))))
      (if (listp categories)
          (pushnew category categories)
          (setf (categories (find-place *global-controller* 'repl-category-filter)) (list category))))))

(defun remove-repl-category (category)
  (with-lock-held ((message-lock *global-controller*))
    (let ((categories (categories (find-place *global-controller* 'repl-category-filter))))
      (when (listp categories)
        (setf (categories (find-place *global-controller* 'repl-category-filter))
              (delete category categories))))))


(unless *global-controller*
  (setf *global-controller* (make-standard-global-controller)))
