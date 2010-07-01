;;;; -*- mode: Lisp -*-
;;;;
;;;; NOTE:
;;;;

(require :cl-json)
(require :cl-ppcre)

(defpackage :bb-util
  (:use
   :cl)
  (:export
   :get-hash-val
   :json-to-hash
   :file-string
   :file-lines
   :flatten
   :kvetch
   :bool-to-int
   :int-to-bool
   :string-merge
   :join-strings
   :ccat
   :generate-flat-hash))

(in-package :bb-util)


;; (defun get-hash-val (hash key)
;;   (let ((new-key
;; 	 (intern (concatenate 'string "+" (symbol-name key) "+") "KEYWORD")))
;;     (gethash new-key hash)))

(defun json-to-hash (json-str)
  ""
  (let ((hash (make-hash-table)))
    (mapcar #'(lambda (x)
		(setf (gethash (car x) hash) (cdr x)))
	    (json:decode-json-from-string json-str))
    hash))

;; From cl-user snippet.
(defun file-string (path)
  "Whole file as string."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

;; From cl-user snippet.
(defun file-lines (path)
  "Whole file as list of strings."
  (with-open-file (s path)
    (loop for line = (read-line s nil nil)
       while line
       collect line into lines
       counting t into line-count
       finally (return (values lines line-count)))))

(defun flatten (xtree)
  "Flatten a tree into a list."
  (labels ((rec (xtree acc)
             (cond ((null xtree) acc)
                   ((atom xtree) (cons xtree acc))
                   (t (rec (car xtree) (rec (cdr xtree) acc))))))
    (rec xtree nil)))

(defun kvetch (obj)
  (format t "~A~%" obj))

(defun join-strings (list &key (with ""))
  "Concatenate a list of strings."
  ;;(format nil "~{~A~^~}" list joiner))
  (format nil (concatenate 'string "~{~A~^" with "~}") list))

(defmacro string-merge (&rest args)
  ""
  `(concatenate 'string ,@args))

;; BUG: needs another version to be smarter when entering dupe keys so
;; things get turned into an array...
(defun generate-flat-hash (list)
  "Creates a sensible hash out of the form '((1 . 2) (3 . 4) (3 . 5)). Values are stored as atoms, unless there is a collision, then a list is created."
  (let ((hash (make-hash-table)))
    (loop for item in list
	  ;;do (setf (gethash (car item) hash) (cdr item)))
	  do (let* ((key (car item))
		    (val (cdr item)))
		    (multiple-value-bind (hval hval-extant-p)
			(gethash key hash)
		      (cond
		       ((and hval-extant-p (listp hval))
			(setf (gethash key hash) (cons val hval)))
		       (hval-extant-p
			(setf (gethash key hash) (list val hval)))
		       (t
			  (setf (gethash key hash) val))))))
    hash))

(defun bool-to-int (bool)
  "Convert a boolen value into an integer."
  (if bool 1 0))

(defun int-to-bool (int)
  "Convert an integer value into a boolean one."
  (if int t nil))

(defmacro ccat (&rest strings)
  "Concatenate a set of strings."
  `(concatenate 'string ,@strings))
