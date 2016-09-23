#|
 This file is a part of Verbose
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(push :verbose *features*)
(defpackage #:verbose
  (:nicknames #:v #:org.shirakumo.verbose)
  (:use :cl :piping)
  (:shadow #:LOG #:ERROR #:WARN #:DEBUG #:TRACE)
  )
