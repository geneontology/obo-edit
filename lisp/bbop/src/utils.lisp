;;;;
;;;; A saner place to keep our Stuff.
;;;;


(require :cl-ppcre)
(defpackage :bbop-utils
  (:use
   :cl)
  (:export 
   :file-string
   :file-lines
   :flatten
   :kvetch
   :string-merge
   :join-strings
   :generate-flat-hash))
(in-package :bbop-utils)


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

(defun flatten (xlist)
  "Flatten a tree into a list."
  (labels ((rec (xlist acc)
             (cond ((null xlist) acc)
                   ((atom xlist) (cons xlist acc))
                   (t (rec (car xlist) (rec (cdr xlist) acc))))))
    (rec xlist nil)))

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
