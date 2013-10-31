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

(defun make-standard-global-controller ()
  (setf *global-controller*
        (make-instance 
         'controller
         :source (build-pipeline (make-instance 'source :name "SOURCE")
                   (valve :name "MAIN-VALVE")
                   (splitter :name "SPLITTER"
                             :targets (build-default-outputs)))))
  (connect-new (get-pipe *global-controller* "INFO")
               (add-pipe *global-controller* 'repl-faucet :name "REPL")))

(unless *global-controller*
  (make-standard-global-controller))

(defun attach-to (level pipe &key category filter (controller *global-controller*))
  (when category
    (setf pipe (make-instance 'filter :next pipe :test #'(lambda (message) (eq category (message-category message))))))
  (when filter
    (setf pipe (make-instance 'filter :next pipe :test filter)))
  (connect-new (get-pipe controller (symbol-name level))
               (add-pipe controller pipe)))

(defun set-repl-level (level &key (faucet-name "REPL") (controller *global-controller*))
  (let ((faucet (get-pipe controller faucet-name)))
    (disconnect (prev faucet) faucet)
    (connect-new (get-pipe controller (symbol-name level)) faucet)))
