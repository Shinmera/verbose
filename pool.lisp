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
  #+atomics-cas-struct-slot
  (loop
    (let ((instances (pool-instances pool))
          (index (pool-index pool)))
      (cond ((< index (length instances))
             (when (atomics:cas (pool-index pool) index (1+ index))
               (return (aref instances index))))
            ((= index (length instances))
             (when (atomics:cas (pool-index pool) index (1+ index))
               ;; We filled, so we are responsible for resizing it.
               (resize-pool pool (* 2 index) :lock T)
               (return (aref instances index))))
            (T
             ;; Re-use the lock as a barrier, then retry
             (bt:with-lock-held ((pool-lock pool)))))))
  #-atomics-cas-struct-slot
  (bt:with-lock-held ((pool-lock pool))
    (let ((instances (pool-instances pool))
          (index (pool-index pool)))
      ;; We filled, so we are responsible for resizing it.
      (loop while (<= (length instances) index)
            do (resize-pool pool (* 2 index) :lock NIL)
               (setf instances (pool-instances pool)))
      (setf (pool-index pool) (1+ index))
      (aref instances index))))

(defun release-instance (pool instance)
  #+atomics-cas-struct-slot
  (loop
    (let ((index (pool-index pool))
          (instances (pool-instances pool)))
      (cond ((= 0 index)
             ;; What the fuck... (double-release?)
             (return))
            ;; FIXME: I don't think this is quite consistent...
            ((= index (length instances))
             ;; What the fuck... (how? wait for a resize maybe?)
             ())
            ((atomics:cas (pool-index pool) index (1- index))
             (return (setf (aref instances index) instance))))))
  #-atomics-cas-struct-slot
  (bt:with-lock-held ((pool-lock pool))
    (let ((index (pool-index pool)))
      (cond ((= 0 index)
             (cl:warn "What the fuck?"))
            (T
             (setf (aref (pool-instances pool) index) instance)
             (setf (pool-index pool) (1- index)))))))
