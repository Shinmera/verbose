#|
  This file is a part of Verbose
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :verbose)

(defun build-default-outputs ()
  (loop with list = (list :TRACE :DEBUG :INFO :WARN :ERROR :SEVERE :FATAL)
     for el = (pop list)
     while el
     collect (let ((curlist (cons el (copy-list list))))
               (make-instance 'filter 
                              :name (format NIL "FILTER-~a" el)
                              :test #'(lambda (message) (find (message-level message) curlist))
                              :next (make-instance 'splitter :name (symbol-name el))))))

(defvar *global-controller* 
  (make-instance 
   'controller
   :source (build-pipeline (make-instance 'source :name "SOURCE")
             (valve :name "MAIN-VALVE")
             (splitter :name "SPLITTER"
                       :targets (build-default-outputs)))))
