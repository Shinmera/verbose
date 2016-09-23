#|
 This file is a part of Verbose
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
