#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

(defvar *repl-faucet-timestamp* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defclass repl-faucet (faucet)
  ())

(defmethod print-object ((faucet repl-faucet) stream)
  (format stream ">>REPL")
  faucet)

(defmethod pass ((faucet repl-faucet) message)
  (format-message faucet message)
  message)

(defmethod format-message ((faucet repl-faucet) (message message))
  (format T "~&LOG: ~a [~5,a] ~{<~a>~}: ~a~%"
          (local-time:format-timestring NIL (message-time message) :format *repl-faucet-timestamp*)
          (message-level message)
          (message-categories message)
          (format-message NIL (message-content message)))
  (finish-output))

(defmethod format-message ((nothing null) message)
  message)

(defmethod format-message ((nothing null) (message function))
  (funcall message))

(defclass file-faucet (faucet)
  ((file :initform NIL :accessor faucet-file)
   (stream :initform NIL :accessor faucet-stream))
  (:default-initargs
   :file #p"verbose.log"))

(defmethod initialize-instance :after ((faucet file-faucet) &key file)
  (setf (faucet-file faucet) file))

(defmethod (setf faucet-file) (file (faucet file-faucet))
  (with-slots (stream) faucet
    (when stream
      (close stream))
    (when file
      (setf stream (open file :direction :output
                              :if-exists :append
                              :if-does-not-exist :create))
      (setf (slot-value faucet 'file) file))))

(defmethod pass ((faucet file-faucet) message)
  (let ((stream (faucet-stream faucet)))
    (when stream
      (format-message stream message)
      (finish-output stream)))
  message)

(defmethod format-message ((stream stream) (message message))
  (format stream "~&~a [~5,a] ~{<~a>~}: ~a~%"
          (local-time:format-timestring NIL (message-time message) :format *repl-faucet-timestamp*)
          (message-level message)
          (message-categories message)
          (format-message NIL (message-content message))))

(defclass rotating-file-faucet (file-faucet)
  ((interval :initform :daily :initarg :interval :accessor interval)
   (last-rotation :initform 0 :accessor last-rotation)
   (template :initform NIL :initarg :template :initarg :file :accessor template)))

(defmethod initialize-instance ((faucet rotating-file-faucet) &key)
  (call-next-method)
  ;; Rotate before the file-faucet opens itself up.
  (rotate faucet))

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
           (setf (faucet-file faucet) new-file))
          (T
           (multiple-value-bind (s m h dd mm yy) (decode-universal-time time)
             (setf (faucet-file faucet)
                   (make-pathname :name (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[ ~a~]"
                                                yy mm dd h m s (pathname-name (template faucet)))
                                  :defaults (template faucet))))))
    (setf (last-rotation faucet) time)))

(defclass category-filter (filter)
  ((categories :initarg :categories :initform T :accessor categories)))

(defmethod pass ((filter category-filter) (message message))
  (when (or (eql (categories filter) T)
            (loop for category in (categories filter)
                  thereis (find category (message-categories message))))
    message))

(defclass category-tree-filter (category-filter)
  ())

(defun matching-tree-category (filter category)
  (let ((category-leaves (split-sequence #\. (string-upcase category)))
        (filter-leaves (split-sequence #\. (string-upcase filter))))
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
                  thereis (find category (message-categories message) :test #'matching-tree-category)))
    message))

(defclass level-filter (filter)
  ((level :initarg :level :initform :info :accessor filtered-level)))

(defmethod pass ((filter level-filter) (message message))
  (when (message-visible message (filtered-level filter))
    message))


;; Backwards compat

(defun make-cron-interval (string)
  (destructuring-bind (m h dm mo dw) (split-sequence #\Space string)
    (flet ((parse (a) (if (string= a "*") '* (parse-integer a))))
      (clon:make-typed-cron-schedule
       :minute (parse m) :hour (parse h) :day-of-month (parse dm) :month (parse mo) :day-of-week (parse dw)))))

(defclass rotating-log-faucet (file-faucet)
  ((time-format :initarg :time-format :accessor time-format)
   (file-template :initarg :file-template :accessor file-template)
   (interval :initarg :interval :accessor interval)
   (scheduler :initform NIL :accessor scheduler)
   (timer :initform NIL :accessor timer))
  (:default-initargs
   :time-format *repl-faucet-timestamp*
   :file-template NIL
   :interval (make-cron-interval "0 0 * * *")))

(defmethod print-object ((faucet rotating-log-faucet) stream)
  (format stream ">>ROTATE(~a)" (interval faucet))
  faucet)

(defmethod initialize-instance :after ((faucet rotating-log-faucet) &key)
  (rotate-log faucet)
  (update-interval faucet))

(defmethod rotate-log ((faucet rotating-log-faucet))
  (when (file-template faucet)
    (setf (faucet-file faucet)
          (merge-pathnames (format NIL "~a-~a"
                                   (local-time:format-timestring NIL (local-time:now) :format (time-format faucet))
                                   (pathname-name (file-template faucet)))
                           (file-template faucet)))
    (ensure-directories-exist (faucet-file faucet)))
  (v:info :verbose.log "Rotated to new file ~a" (faucet-file faucet)))

(defmethod update-interval ((faucet rotating-log-faucet) &optional (interval (interval faucet)))
  (when (timer faucet) (trivial-timers:unschedule-timer (timer faucet)))
  (when (stringp interval) (setf interval (make-cron-interval interval)))
  (setf (interval faucet) interval
        (scheduler faucet) (clon:make-scheduler interval)
        (timer faucet) (clon:schedule-function #'(lambda () (rotate-log faucet)) (scheduler faucet))))

(defmethod stop-rotation ((faucet rotating-log-faucet))
  (when (timer faucet)
    (trivial-timers:unschedule-timer (timer faucet)))
  (setf (scheduler faucet) NIL
        (timer faucet) NIL))
