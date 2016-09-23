#|
 This file is a part of Verbose
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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

