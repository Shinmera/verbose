#|
  This file is a part of Verbose
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.verbose)

;;
;; REPL
;;

(defvar *repl-faucet-timestamp* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2))
  "Default timestamp format of the repl faucet. See LOCAL-TIME:FORMAT-TIMESTRING.")

(defgeneric format-message (faucet message)
  (:documentation "Wrapper around pass to potentially be overwritten if the output format should be changed.
This serves a dual purpose: First it allows writers of faucets to customise
their output representation. Secondly, it coerces the actual message content
into a string to output.

By default, the following methods are defined:
(REPL-FAUCET MESSAGE) print to the repl, using (NIL content) to coerce the content.
(STREAM MESSAGE)
(NIL CONDITION)       if *VERBOSE-CONDITIONS* is T, (DISSECT:PRESENT message) is
                      used as the output. Otherwise, the message itself is returned
                      directly.
(NIL FUNCTION)        call the message function and return its result.
(NIL T)               simply return the message."))

(defclass repl-faucet (faucet)
  () (:documentation "A simple logging faucet that prints log messages to the *standard-output*"))

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

;;
;; FILE
;;

(defclass file-faucet (faucet)
  ((file :initarg :file :initform #p"verbose.log" :accessor faucet-file)))

(defmethod pass ((faucet file-faucet) message)
  (when (faucet-file faucet)
    (with-open-file (stream (faucet-file faucet) :direction :output
                                                 :if-exists :append
                                                 :if-does-not-exist :create)
      (format-message stream message)
      (finish-output stream)))
  message)

(defmethod format-message ((stream stream) (message message))
  (format stream "~&~a [~5,a] ~{<~a>~}: ~a~%"
          (local-time:format-timestring NIL (message-time message) :format *repl-faucet-timestamp*)
          (message-level message)
          (message-categories message)
          (format-message NIL (message-content message))))

;;
;; CRON
;;

(defun make-cron-interval (string)
  "Parse a cron interval."
  (destructuring-bind (m h dm mo dw) (split-sequence #\Space string)
    (flet ((parse (a) (if (string= a "*") '* (parse-integer a))))
      (clon:make-typed-cron-schedule
       :minute (parse m) :hour (parse h) :day-of-month (parse dm) :month (parse mo) :day-of-week (parse dw)))))

(defclass rotating-log-faucet (file-faucet)
  ((time-format :initarg :time-format :initform *repl-faucet-timestamp* :accessor time-format)
   (current-file :initform NIL :accessor current-file)
   (interval :initarg :interval :initform (make-cron-interval "0 0 * * *") :accessor interval)
   (scheduler :initform NIL :accessor scheduler)
   (timer :initform NIL :accessor timer))
  (:documentation "A file logger that rotates at the given (cron) interval."))

(defmethod print-object ((faucet rotating-log-faucet) stream)
  (format stream ">>ROTATE(~a)" (interval faucet))
  faucet)

(defmethod initialize-instance :after ((faucet rotating-log-faucet) &rest args)
  (declare (ignore args))
  (rotate-log faucet)
  (update-interval faucet))

(defgeneric rotate-log (rotating-log-faucet)
  (:documentation "Initiate a log rotation immediately. This does not influence the automatic rotation interval."))
(defmethod rotate-log ((faucet rotating-log-faucet))
  (setf (current-file faucet)
        (merge-pathnames (format NIL "~a-~a"
                                 (local-time:format-timestring NIL (local-time:now) :format (time-format faucet))
                                 (pathname-name (faucet-file faucet)))
                         (faucet-file faucet)))
  (ensure-directories-exist (faucet-file faucet))
  (v:info :verbose.log "Rotated to new file ~a" (current-file faucet)))

(defgeneric update-interval (rotating-log-faucet &optional (cron-interval))
  (:documentation "Change the rotation interval. cron-interval should either be a cron-parsable string or a clon:cron-schedule."))
(defmethod update-interval ((faucet rotating-log-faucet) &optional (interval (interval faucet)))
  (when (timer faucet) (trivial-timers:unschedule-timer (timer faucet)))
  (when (stringp interval) (setf interval (make-cron-interval interval)))
  (setf (interval faucet) interval
        (scheduler faucet) (clon:make-scheduler interval)
        (timer faucet) (clon:schedule-function #'(lambda () (rotate-log faucet)) (scheduler faucet))))

(defgeneric stop-rotation (rotating-log-faucet)
  (:documentation "Stops the rotation. Logging will still continue on the current file."))
(defmethod stop-rotation ((faucet rotating-log-faucet))
  (when (timer faucet)
    (trivial-timers:unschedule-timer (timer faucet)))
  (setf (scheduler faucet) NIL
        (timer faucet) NIL))

;;
;; Filters
;;

(defclass category-filter (filter)
  ((categories :initarg :categories :initform T :accessor categories))
  (:documentation "A simple pipe filter that only lets through matching categories."))

(defmethod pass ((filter category-filter) (message message))
  (when (or (eql (categories filter) T)
            (loop for category in (categories filter)
                  thereis (find category (message-categories message))))
    message))

(defclass category-tree-filter (category-filter) ()
  (:documentation "A pipe filter that only lets messages through whose category matches by tree."))

(defun %matching-tree-category (filter category)
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
                  thereis (find category (message-categories message) :test #'%matching-tree-category)))
    message))

(defclass level-filter (filter)
  ((level :initarg :level :initform :info :accessor filtered-level))
  (:documentation "A simple pipe filter that only lets through messages that are above a certain level."))

(defmethod pass ((filter level-filter) (message message))
  (when (message-visible message (filtered-level filter))
    message))
