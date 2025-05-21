(in-package #:org.shirakumo.verbose)

(defstruct (pool
            (:predicate NIL)
            (:copier NIL)
            (:constructor %make-pool (instances constructor)))
  (instances NIL :type simple-vector)
  (index 0 :type (unsigned-byte 32))
  (constructor NIL :type function)
  (lock (bt:make-lock "verbose pool")))

(defmethod print-object ((pool pool) stream)
  (print-unreadable-object (pool stream :type T :identity T)
    (format stream "~d/~d" (pool-index pool) (length (pool-instances pool)))))

(defun make-pool (constructor &optional (initial-size 128))
  (let ((instances (make-array initial-size)))
    (map-into instances constructor)
    (%make-pool instances constructor)))

(defun check-pool (pool)
  (bt:with-lock-held ((pool-lock pool))
    (loop with cache = (make-hash-table :test 'eq)
          for i from 0
          for instance across (pool-instances pool)
          for existing = (gethash instance cache)
          do (cond ((null instance)
                    (cl:error "Missing entry in ~d" i))
                   (existing
                    (cl:error "Duplicate entry in ~d and ~d: ~a" existing i instance))
                   (T
                    (setf (gethash instance cache) i))))
    pool))

(defun clear-pool (pool)
  (bt:with-lock-held ((pool-lock pool))
    ;; The order is no longer guaranteed to be consistent
    ;; and due to out of order release/draw, some objects may
    ;; not be present at all, or present multiple times, we have
    ;; to refill the pool entirely.
    (setf (pool-index pool) 0)
    (map-into (pool-instances pool) (pool-constructor pool))
    pool))

(defun resize-pool (pool new-size &key (lock T))
  (flet ((resize ()
           (let* ((old (pool-instances pool))
                  (new (adjust-array old new-size)))
             (loop for i from (length old) below new-size
                   do (setf (aref new i) (funcall (pool-constructor pool))))
             (setf (pool-instances pool) new))))
    (if lock
        (bt:with-lock-held ((pool-lock pool))
          (resize))
        (resize))))

(defun draw-instance (pool)
  (declare (type pool pool))
  (declare (optimize speed))
  (bt:with-lock-held ((pool-lock pool))
    (let ((instances (pool-instances pool))
          (index (pool-index pool)))
      (declare (type simple-vector instances))
      ;; We filled, so we are responsible for resizing it.
      (loop while (<= (length instances) index)
            do (setf instances (resize-pool pool (* 2 index) :lock NIL)))
      (setf (pool-index pool) (1+ index))
      (shiftf (aref instances index) NIL))))

(defun release-instance (pool instance)
  (declare (type pool pool))
  (declare (optimize speed))
  (bt:with-lock-held ((pool-lock pool))
    (let ((index (pool-index pool)))
      (cond ((= 0 index)
             (cl:warn "What the fuck?"))
            (T
             (setf (aref (pool-instances pool) (1- index)) instance)
             (setf (pool-index pool) (1- index)))))))
