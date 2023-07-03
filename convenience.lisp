(in-package #:org.shirakumo.verbose)

(defun remove-global-controller ()
  (when *global-controller*
    (stop *global-controller*)
    (setf *global-controller* NIL)))

(defun restart-global-controller ()
  (remove-global-controller)
  (setf *global-controller* (make-standard-global-controller)))

(defun repl-level (&optional (controller *global-controller*))
  (level (find-place controller 'repl-level-filter)))

(defun (setf repl-level) (level &optional (controller *global-controller*))
  (setf (level (find-place controller 'repl-level-filter)) level))

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
  (setf (output (find-place controller 'repl-faucet)) standard-output))

(defun add-pipe (&rest segments)
  (let ((controller (if (typep (first segments) 'controller)
                        (pop segments)
                        *global-controller*)))
    (with-controller-lock (controller)
      (let ((pipe (make-pipe)))
        (dolist (segment segments)
          (insert segment pipe))
        (add-segment controller pipe)))))

(defmacro define-pipe ((&optional (pipeline '*global-controller*) place) &body segments)
  (let ((names (loop for i from 0
                     for segment in segments
                     for name = (getf (rest segment) :name)
                     when name collect (list i name)))
        (pipe (gensym "PIPE"))
        (parent (gensym "PARENT"))
        (c (gensym "C")))
    `(let ((,parent ,pipeline)
           (,pipe (make-pipe)))
       ,@(loop for (type . args) in segments
               collect `(insert (make-instance ',type ,@(removef args :name)) ,pipe))
       (add-segment ,parent ,pipe ,place)
       ,(when names
          `(let ((,c (1- (length (pipeline ,parent)))))
             ,@(loop for (i name) in names
                     collect `(set-name ,parent (list ,c ,i) ,name))))
       ,parent)))

(defun make-standard-global-controller (&rest initargs)
  (let ((pipeline (apply #'make-instance 'controller initargs)))
    (define-pipe (pipeline)
      (level-filter :name 'repl-level-filter)
      (category-tree-filter :name 'repl-category-filter)
      (repl-faucet :name 'repl-faucet))
    pipeline))

(setf *global-controller*
      (make-standard-global-controller :dont-start (find :verbose-no-init *features*)))
