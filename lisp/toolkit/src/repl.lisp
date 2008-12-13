;;;;
;;;; Something for manipulating the REPL environment later on...
;;;;


(defpackage :toolkit-repl
  (:use :cl))
(in-package :toolkit-repl)

;;
(defmacro verbose-form (form)
  `(progn
     (format t "Will do: ~a...~%" ',form)
     (time ,form) ; ,form
     (format t "Done: ~a~%" ',form)))

(defmacro chatty (&rest body)
  (let ((rewrite (loop for f in body
		      collect `(verbose-form ,f))))
  `(progn ,@rewrite)))

;; TODO: get this from a standard input.
;; TODO: Also, get the first word and check it against a clean list.
(defun sub-repl (string)
  ""
  (princ (eval (read-from-string
		(concatenate 'string "(" string ")")))))
