(in-package #:org.shirakumo.verbose)

(defvar *ansi-colors*)

(defmethod stop ((segment segment)))

(defmethod stop ((pipeline pipeline))
  (stop (pipeline pipeline)))

(defmethod stop ((pipeline vector))
  (map NIL #'stop pipeline))

(defmethod start ((segment segment)))

(defmethod start ((pipeline pipeline))
  (start (pipeline pipeline)))

(defmethod start ((pipeline vector))
  (map NIL #'start pipeline))

(defclass stream-faucet (faucet)
  ((output :initarg :output :initform NIL :accessor output)))

(defmethod pass ((faucet stream-faucet) message)
  (when (output faucet)
    (format-message faucet message))
  message)

(defmethod format-message :before ((faucet stream-faucet) thing)
  (fresh-line (output faucet)))

(defmethod format-message :after ((faucet stream-faucet) thing)
  (terpri (output faucet))
  (force-output (output faucet)))

(defmethod format-message ((faucet stream-faucet) (message message))
  (format-message (output faucet) message))

(defmethod stop ((faucet stream-faucet))
  (close (output faucet)))

(defun ansi-term-p ()
  (if (boundp '*ansi-colors*)
      *ansi-colors*
      (not (find (or (getenv "TERM") "") '("" "dumb" "linux" "emacs") :test #'string-equal))))

(defclass repl-faucet (stream-faucet)
  ((output :initform *standard-output*)
   (ansi-colors :initform (ansi-term-p) :accessor ansi-colors)))

(defmethod print-object ((faucet repl-faucet) stream)
  (format stream ">>REPL")
  faucet)

(defmethod format-message ((faucet repl-faucet) (message message))
  (if (ansi-colors faucet)
      (let ((stream (output faucet)))
        (macrolet ((f (color format &rest args)
                     `(progn
                        ,@(when color `((format stream "~c[38;5;~am" (code-char #x1b) ,color)))
                        ,(if (stringp format)
                             `(format stream ,format ,@args)
                             `(progn ,format ,@args))
                        ,@(when color `((format stream "~c[0m" (code-char #x1b))))))
                   (fb (fg bg format &rest args)
                     `(progn
                        (format stream "~c[38;5;~am~c[48;5;~am" (code-char #x1b) ,fg (code-char #x1b) ,bg)
                        ,(if (stringp format)
                             `(format stream ,format ,@args)
                             `(progn ,format ,@args))
                        (format stream "~c[0m" (code-char #x1b)))))
          (f 7 (format-time :timestamp (timestamp message) :stream stream))
          (f 8 " [")
          (f (case (level message)
               (:trace 8)
               (:debug 7)
               (:info NIL)
               (:warn 9)
               (:error 1)
               (:severe 11)
               (:fatal 11)) "~5,a" (level message))
          (f 8 "] ")
          (dolist (category (categories message))
            (f 8 "<")
            (f NIL "~a" category)
            (f 8 ">"))
          (f 8 ": ")
          (flet ((msg (fg &optional bg)
                   (if bg
                       (fb fg bg (format-message stream (content message)))
                       (f fg (format-message stream (content message))))))
            (case (level message)
              (:trace (msg 8))
              (:debug (msg 7))
              (:info (msg NIL))
              (:warn (msg 9))
              (:error (msg 1))
              (:severe (msg 11))
              (:fatal (msg 0 11))))))
      (call-next-method)))

(defmethod stop ((faucet repl-faucet)))

(defclass file-faucet (stream-faucet)
  ((file :initform NIL :accessor file))
  (:default-initargs
   :file #p"verbose.log"))

(defmethod initialize-instance :after ((faucet file-faucet) &key file)
  (setf (file faucet) file))

(defmethod (setf file) (file (faucet file-faucet))
  (with-slots (output) faucet
    (when output
      (close output))
    (when file
      (setf output (open file :direction :output
                              :if-exists :append
                              :if-does-not-exist :create
                              :external-format :utf-8
                              #+ccl :sharing #+ccl NIL))
      (setf (slot-value faucet 'file) file))))

(defmethod start ((faucet file-faucet))
  (setf (file faucet) (file faucet)))

(defclass rotating-file-faucet (file-faucet)
  ((interval :initform NIL :accessor interval)
   (last-rotation :initform 0 :accessor last-rotation)
   (template :initform NIL :initarg :template :accessor template))
  (:default-initargs
   :interval :daily
   :file NIL))

(defmethod initialize-instance :after ((faucet rotating-file-faucet) &key interval)
  (setf (interval faucet) interval)
  (rotate faucet))

(defmethod (setf interval) (value (faucet rotating-file-faucet))
  (ecase value
    ((:hourly :daily :monthly :weekly)
     (setf (slot-value faucet 'interval) value))))

(defmethod pass :before ((faucet rotating-file-faucet) thing)
  (let ((pre (last-rotation faucet))
        (now (get-universal-time)))
    (when (multiple-value-bind (s m h dd mm yy dow) (decode-universal-time now)
            (multiple-value-bind (ps pm ph pdd pmm pyy pdow) (decode-universal-time pre)
              (ecase (interval faucet)
                (:hourly
                 (or (/= ph h) (/= pdd dd) (/= pmm mm) (/= pyy yy)))
                (:daily
                 (or (/= pdd dd) (/= pmm mm) (/= pyy yy)))
                (:monthly
                 (or (/= pmm mm) (/= pyy yy)))
                (:weekly
                 (< (* 60 60 24 7) (- (get-universal-time) (last-rotation faucet)))))))
      (rotate faucet))))

(defmethod rotate ((faucet rotating-file-faucet) &optional new-file)
  (let ((time (setf (last-rotation faucet) (get-universal-time))))
    (cond (new-file
           (setf (file faucet) new-file))
          (T
           (multiple-value-bind (s m h dd mm yy) (decode-universal-time time)
             (setf (file faucet)
                   (make-pathname :name (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[ ~a~]"
                                                yy mm dd h m s (when (template faucet) (pathname-name (template faucet))))
                                  :defaults (template faucet))))))
    (setf (last-rotation faucet) time)))

(defclass level-filter (filter)
  ((level :initform :info :accessor level))
  (:default-initargs
   :level :info))

(defmethod initialize-instance :after ((filter level-filter) &key level)
  (setf (level filter) level))

(defmethod (setf level) :before (level (filter level-filter))
  (unless (or (integerp level)
              (find level *levels* :key #'cdr))
    (cl:error "~a is neither a level in *LEVELS*, nor an integer." level)))

(defmethod pass ((filter level-filter) (message message))
  (let ((level (level filter)))
    (when (<= (if (integerp level)
                  level
                  (position level *levels* :key #'cdr))
              (position (level message) *levels* :key #'cdr))
      message)))

(defclass category-filter (filter)
  ((categories :initarg :categories :initform T :accessor categories)))

(defmethod pass ((filter category-filter) (message message))
  (when (or (eql (categories filter) T)
            (loop for category in (categories filter)
                  thereis (find category (categories message))))
    message))

(defclass category-tree-filter (category-filter)
  ())

(defun matching-tree-category (filter category)
  (let ((category-leaves (split (string-upcase category) #\.))
        (filter-leaves (split (string-upcase filter) #\.)))
    (loop for catl in category-leaves
       for fill in filter-leaves
       do (cond
            ((or (string= catl "*")
                 (string= fill "*"))
             (return T))
            ((not (string= catl fill))
             (return NIL)))
       finally (return (>= (length category-leaves)
                           (length filter-leaves))))))

(defmethod pass ((filter category-tree-filter) (message message))
  (when (or (eql (categories filter) T)
            (loop for category in (categories filter)
                  thereis (find category (categories message) :test #'matching-tree-category)))
    message))
