#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defun make-standard-global-controller ()
  (let ((pipeline (make-instance 'controller))
        (pipe (make-pipe)))
    (insert (make-instance 'level-filter) pipe)
    (insert (make-instance 'category-tree-filter) pipe)
    (insert (make-instance 'repl-faucet) pipe)
    (add-segment pipeline pipe)
    (set-name pipeline '(0 0) 'repl-level-filter)
    (set-name pipeline '(0 1) 'repl-category-filter)
    (set-name pipeline '(0 2) 'repl-faucet)
    pipeline))

(defun remove-global-controller ()
  (when *global-controller*
    (stop-controller *global-controller*)
    (setf *global-controller* NIL)))

(defun restart-global-controller ()
  (remove-global-controller)
  (setf *global-controller* (make-standard-global-controller)))

(defun repl-level (&optional (controller *global-controller*))
  (filtered-level (find-place controller 'repl-level-filter)))

(defun (setf repl-level) (level &optional (controller *global-controller*))
  (setf (filtered-level (find-place controller 'repl-level-filter)) level))

(defun repl-categories (&optional (controller *global-controller*))
  (with-controller-lock ()
    (categories (find-place controller 'repl-category-filter))))

(defun (setf repl-categories) (categories &optional (controller *global-controller*))
  (with-controller-lock ()
    (setf (categories (find-place controller 'repl-category-filter))
          categories)))

(defun add-repl-category (&rest category)
  (let ((controller (if (typep (first category) 'controller)
                        (pop category)
                        *global-controller*)))
    (with-controller-lock (controller)
      (let ((filter (find-place controller 'repl-category-filter)))
        (when filter
          (if (listp (categories filter))
              (setf (categories filter) (append category (categories filter)))
              (setf (categories filter) category)))))))

(defun remove-repl-category (&rest category)
  (let ((controller (if (typep (first category) 'controller)
                        (pop category)
                        *global-controller*)))
    (with-controller-lock (controller)
      (let ((filter (find-place controller 'repl-category-filter)))
        (when (and filter (listp (categories filter)))
          (setf (categories filter) (remove-if (lambda (e) (find e category))
                                               (categories filter))))))))

(defun output-here (&optional (standard-output *standard-output*) (controller *global-controller*))
  (setf (shared-instance '*standard-output* controller) standard-output))

(defun add-pipe (&rest segments)
  (let ((controller (if (typep (first segments) 'controller)
                        (pop segments)
                        *global-controller*)))
    (with-controller-lock (controller)
      (let ((pipe (make-pipe)))
        (dolist (segment segments)
          (insert segment pipe))
        (add-segment controller pipe)))))

(unless (find :verbose-no-init *features*)
  (unless *global-controller*
    (setf *global-controller* (make-standard-global-controller))))
