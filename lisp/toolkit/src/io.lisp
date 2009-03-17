;;;;
;;;; File 
;;;;


(defpackage :toolkit-io
  (:use :cl
	:cl-ppcre
	:cl-fad)
  (:export :slurp
	   :read-file
	   :write-file
	   :make-splitter
	   :make-filter
	   :easy-filter))
(in-package :toolkit-io)


(defun slurp (file-string &key (function #'(lambda (x) x)))
  "Read in a file line by line, exectute a function on each line as a
string and collect the output into a list. The default function is to
just collect the strings into a list."
  (with-open-file (stream file-string)
    (loop
       for line = (read-line stream nil)
       while line
       collect (funcall function line))))

;; Needs cl-ppcre
(defun make-splitter (&optional (regexp-text "\\t+"))
  "Define a function that splits a line into. Default to tabs."
  #'(lambda (line) (cl-ppcre::split regexp-text line)))

(defun read-file (file-string)
  "Return a list of a structured \"print\" outputted file."
  (with-open-file (stream file-string)
    (read stream nil)))

;; Usage: (funcall (make-filter (not (eq (length x) 3))) list)
(defmacro make-filter (&rest tests)
  "Define a function that removes items from a list if found to be
  true. Takes multiple form arguments that use x as an argument."
  `#'(lambda (list)
       (remove-if #'(lambda (x)
		      (or ,@tests))
		  list)))

(defun easy-filter (list &key (comment-regexp-string "^#") (length 3))
  "Drop items from list if are commenty or don't have enough things."
  (remove-if #'(lambda (x)
 		 (or
		  (not (eq (length x) length))
		  (cl-ppcre::scan comment-regexp-string (car x))))
	     list))

;; :if-exists :supersede)
(defun write-file (object file-string)
  "Writes an object to a lisp-friendly format."
  (with-open-file (stream file-string :direction :output)
    (print object stream)))

;;  (defun writey (list file-string)
;;    "Writes a list to a lisp-friendly format."
;;    ;; :if-exists :supersede)
;;    (with-open-file (stream file-string :direction :output)
;;      (loop
;; 	for item in list
;; 	do (print item stream))))
