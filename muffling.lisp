(in-package #:org.shirakumo.verbose)

(defvar *muffled-categories* NIL)

(defmethod pass ((controller controller) (message message))
  (unless (find T *muffled-categories*)
    (when *muffled-categories*
      (setf (categories message)
            (loop for category in (categories message)
                  unless (loop for filter in *muffled-categories*
                               thereis (matching-tree-category filter category))
                  collect category)))
    (when (categories message)
      (call-next-method))))

(defmacro with-muffled-logging ((&optional (category T) &rest more-categories) &body body)
  `(let ((*muffled-categories* (list* ,category ,@more-categories *muffled-categories*)))
     ,@body))

(defmacro with-muffled-logging* ((&optional (category T) &rest more-categories) &body body)
  (let ((original-categories (gensym (string 'original-categories))))
    `(let ((,original-categories *muffled-categories*))
       (setf *muffled-categories* (list* ,category ,@more-categories *muffled-categories*))
       (unwind-protect
            (progn ,@body)
         (setf *muffled-categories* ,original-categories)))))

