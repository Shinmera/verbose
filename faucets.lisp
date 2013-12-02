#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defvar *repl-faucet-timestamp* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defgeneric format-message (faucet message)
  (:documentation "Wrapper around pass to potentially be overwritten if the output format should be changed."))

(defclass repl-faucet (faucet)
  () (:documentation "A simple logging faucet that prints log messages to the *standard-output*"))

(defmethod print-self ((faucet repl-faucet) stream)
  (format stream ">>~:[~;~:*~a~]|REPL" (piping:name faucet)))

(defmethod pass ((faucet repl-faucet) message)
  (format-message faucet message))

(defmethod format-message ((faucet repl-faucet) message)
  (format T "~&LOG: ~a [~5,a] <~a>: ~a~%"
          (local-time:format-timestring NIL (message-time message) :format *repl-faucet-timestamp*)
          (message-level message)
          (message-category message)
          (message-content message)))

(defclass cron-interval ()
  ((minute :initarg :m :initform :* :accessor minute)
   (hour :initarg :h :initform :* :accessor hour)
   (day-of-month :initarg :dom :initform :* :accessor day-of-month)
   (month :initarg :month :initform :* :accessor month)
   (day-of-week :initarg :dow :initform :* :accessor day-of-week))
  (:documentation "A cron-like interval for rotations."))

(defmethod print-object ((cron-interval cron-interval) stream)
  (print-unreadable-object (cron-interval stream :type T)
    (format stream "~a ~a ~a ~a ~a" 
            (minute cron-interval)
            (hour cron-interval)
            (day-of-month cron-interval)
            (month cron-interval)
            (day-of-week cron-interval))))

(defun make-cron-interval (string)
  (flet ((parse-or-any (part)
           (if (string-equal part "*") :* (parse-integer part))))
    (let ((parts (split-sequence #\Space string)))
      (assert (= (length parts) 5) () "Interval requires exactly 5 parts.")
      (make-instance 'cron-interval 
                     :m (parse-or-any (first parts))
                     :h (parse-or-any (second parts))
                     :dom (parse-or-any (third parts))
                     :month (parse-or-any (fourth parts))
                     :dow (parse-or-any (fifth parts))))))

(defun start-cron-job (interval method)
  (flet ((convert (part)
           (let ((part (slot-value interval part)))
             (if (eq part :*) :every part))))
    (cl-cron:make-cron-job 
     method 
     :minute (convert 'minute)
     :hour (convert 'hour)
     :day-of-month (convert 'day-of-month)
     :month (convert 'month)
     :day-of-week (convert 'day-of-week))))

(defgeneric rotate-log (rotating-log-faucet)
  (:documentation "Create a new log file."))

(defgeneric update-interval (rotating-log-faucet interval)
  (:documentation "Change the rotation interval"))

(defclass rotating-log-faucet (faucet)
  ((interval :initarg :interval :initform (make-cron-interval "0 0 * * *") :accessor interval)
   (cron-job :initform NIL :accessor cron-job)
   (time-format :initarg :time-format :initform *repl-faucet-timestamp* :accessor time-format)
   (file :initarg :file :initform #p"verbose.log" :accessor faucet-file)
   (current-file :initform NIL :accessor current-file)
   (stream :initform NIL :accessor faucet-stream))
  (:documentation "A file logger that rotates at the given (cron) interval."))

(defmethod print-self ((faucet rotating-log-faucet) stream)
  (format stream ">>~:[~;~:*~a~]|ROTATE(~a)" (piping::name faucet) (interval faucet)))

(defmethod initialize-instance :after ((faucet rotating-log-faucet) &rest args)
  (declare (ignore args))
  (rotate-log faucet)
  (update-interval faucet (interval faucet)))

(defmethod pass ((faucet rotating-log-faucet) message)
  (let ((stream (faucet-stream faucet)))
    (when (and (streamp stream) (open-stream-p stream))
      (format-message faucet message)
      (finish-output stream))))

(defmethod format-message ((faucet rotating-log-faucet) message)
  (format (faucet-stream faucet) "~a [~5,a] <~a>: ~a~%"
          (local-time:format-timestring NIL (message-time message) :format *repl-faucet-timestamp*)
          (message-level message)
          (message-category message)
          (message-content message)))

(defmethod rotate-log ((faucet rotating-log-faucet))
  (let ((stream (faucet-stream faucet)))
    (when (and (streamp stream) (open-stream-p stream))
      (close stream)
      (info :LOGGING.ROTATE "Closed log file: ~a" (current-file faucet))))
  (let* ((file (faucet-file faucet))
         (path (make-pathname :name (format NIL "~a-~a" (local-time:format-timestring T (local-time:now) :format (time-format faucet)) (pathname-name file))
                              :defaults file)))
    (setf (faucet-stream faucet) (open path :direction :output :if-does-not-exist :create :if-exists :append))
    (setf (current-file faucet) path)
    (info :LOGGING.ROTATE "Opened log file: ~a" path)))

(defmethod update-interval ((faucet rotating-log-faucet) (interval cron-interval))
  (if (cron-job faucet) (cl-cron:delete-cron-job (cron-job faucet)))
  (setf (interval faucet) interval
        (cron-job faucet) (start-cron-job interval #'(lambda () (rotate-log faucet)))))
