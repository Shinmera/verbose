(in-package #:org.shirakumo.verbose)

(defun removef (plist &rest keys)
  (loop for (key val) on plist by #'cddr
        for test = (find key keys)
        unless test collect key
        unless test collect val))

(defun split (string on)
  (declare (optimize speed))
  (declare (type character on))
  (declare (type string string))
  (let ((parts ()))
    (loop with out = (make-string-output-stream)
          for c across string
          do (if (eql c on)
                 (push (get-output-stream-string out) parts)
                 (write-char c out))
          finally (push (get-output-stream-string out) parts))
    (nreverse parts)))

(defun getenv (x)
  #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+cmucl (unix:unix-getenv x)
  #+lispworks (lispworks:environment-variable x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl clasp clisp ecl xcl allegro clozure cmucl lispworks sbcl)
  NIL)

(defvar *timestamp-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defun format-time (&key (timestamp (get-universal-time))
                         (format *timestamp-format*)
                         (stream NIL))
  (etypecase stream
    (stream
     (multiple-value-bind (ss mm hh d m y) (decode-universal-time timestamp)
       (loop for spec in format
             do (etypecase spec
                  (character (write-char spec stream))
                  (string (write-string spec stream))
                  (cons (destructuring-bind (part length) spec
                          (format stream "~v,'0d" length
                                  (ecase part
                                    ((:year :y) y)
                                    ((:month :m) m)
                                    ((:day :d) d)
                                    ((:hour :h :hh) hh)
                                    ((:min :mm) mm)
                                    ((:sec :s :ss) ss)))))))))
    (null
     (with-output-to-string (stream)
       (format-time :timestamp timestamp :format format :stream stream)))
    ((eql T)
     (format-time :timestamp timestamp :format format :stream *standard-output*))))
