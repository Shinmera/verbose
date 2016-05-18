#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defun make-standard-global-controller ()
  "Creates a new standard global controller construct with a setup pipeline."
  (let ((pipeline (make-instance 'controller))
        (pipe (make-pipe)))
    (insert (make-instance 'predicate-filter :predicate #'(lambda (message) (message-visible message (shared-instance 'repl-level)))) pipe)
    (insert (make-instance 'category-tree-filter) pipe)
    (insert (make-instance 'repl-faucet) pipe)
    (add-segment pipeline pipe)
    (set-name pipeline (list 0 0) 'repl-level-filter)
    (set-name pipeline (list 0 1) 'repl-category-filter)
    (set-name pipeline (list 0 2) 'repl-faucet)
    (setf (gethash 'repl-level (shares pipeline)) :INFO)
    pipeline))

(defun remove-global-controller ()
  "Attempts to destroy the thread of the controller and remove it."
  (when *global-controller*
    (stop-controller *global-controller*)
    (setf *global-controller* NIL)))

(defun restart-global-controller ()
  "Removes the controller and creates a new one in its place."
  (remove-global-controller)
  (setf *global-controller* (make-standard-global-controller)))

(defun repl-level ()
  "Returns the current logging level of the standard repl faucet."
  (shared-instance 'repl-level))

(defgeneric (setf repl-level) (level)
  (:documentation "Sets the logging level of the standard repl faucet.")
  (:method (level)
    (setf (shared-instance 'repl-level) level)))

(defun repl-categories ()
  "Returns the list of allowed log categories of the standard category-tree-filter.
If NIL is returned, anything is passed."
  (with-controller-lock ()
    (categories (find-place *global-controller* 'repl-category-filter))))

(defgeneric (setf repl-categories) (categories)
  (:documentation "Sets the list of allowed log categories of the standard category-tree-filter.")
  (:method (categories)
    (with-controller-lock ()
      (setf (categories (find-place *global-controller* 'repl-category-filter))
            categories))))

(defun add-repl-category (&rest category)
  "Add a new repl category to allow."
  (with-controller-lock ()
    (let ((filter (find-place *global-controller* 'repl-category-filter)))
      (when filter
        (if (listp (categories filter))
            (setf (categories filter) (append category (categories filter)))
            (setf (categories filter) category))))))

(defun remove-repl-category (&rest category)
  "Remove an existing repl category."
  (with-controller-lock ()
    (let ((filter (find-place *global-controller* 'repl-category-filter)))
      (when (and filter (listp (categories filter)))
        (setf (categories filter) (remove-if (lambda (e) (find e category))
                                             (categories filter)))))))

(defun output-here (&optional (standard-output *standard-output*))
  "Set the *standard-output* shared instance to STANDARD-OUTPUT, effectively
redirecting output from the logger thread to the specified stream."
  (setf (shared-instance '*standard-output*) standard-output))

(defun add-pipe (&rest segments)
  "Add a new pipe with the given segments."
  (with-controller-lock ()
    (let ((pipe (make-pipe)))
      (dolist (segment segments)
        (insert segment pipe))
      (add-segment *global-controller* pipe))))

(unless (find :verbose-no-init *features*)
  (unless *global-controller*
    (setf *global-controller* (make-standard-global-controller))))
