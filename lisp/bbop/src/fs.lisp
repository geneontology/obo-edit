;;;; -*- mode: Lisp -*-
;;;;
;;;; A collection of day-to-day utilites and functions for getting
;;;; along with the filesystem in a lisp environment.
;;;;
;;;; Abandoning most of this: http://weitz.de/cl-fad/ Someone has
;;;; already been here and done most of this: cl-fad
;;;;
;;;; directory-pathname-p
;;;; pathname-as-directory
;;;; pathname-as-file
;;;; file-exists-p
;;;; directory-exists-p
;;;; list-directory
;;;; walk-directory
;;;; delete-directory-and-files
;;;; copy-file
;;;; copy-stream 
;;;;


(defpackage :bb-fs
  (:use :cl
	:cl-fad)
  (:export :pwd
	   :cd
	   :list-files
	   :make-directory
	   :list-directories))
(in-package :bb-fs)

;;;
;;; Filesystem
;;;

;; Not really needed, just practice.
(defun pwd ()
  #+(or sbcl)
  (truename ".")
  #-(or sbcl)
  (error "get-cwd not implemented"))

;; Not really needed, just practice.
;;(defun cd (&optional (path "/home/sjcarbon" path-supplied-p))
(defun cd (&optional (path *default-pathname-defaults*))

  #+(or sbcl)
  ;;(if (path-supplied-p)
  (setf *default-pathname-defaults* (truename path))
  ;;    (setf *default-pathname-defaults* (truename path)))

  #-(or sbcl)
  (error "get-cwd not implemented"))

;;
(defun list-files (dir)
  "List all of the files in directory <dir>."
  (bb-list:filter (cl-fad:list-directory dir)
		       #'(lambda (x) (cl-fad:directory-exists-p x))))

;;
(defun list-directories (dir)
  "List all of the directories in directory <dir>."
  (bb-list:filter (cl-fad:list-directory dir)
		  #'(lambda (x) (not (cl-fad:directory-exists-p x)))))

;; Cut up path, go back to root, and drill down where needed.
(defun make-directory (dir)
  "Make a directory. If it's not top-level drill in (like mkdir -p)"
  ;; Get everything up to it, but possibly not including...
  #+(or sbcl)
  (if (not (cl-fad:directory-exists-p dir))
      (progn
	(ensure-directories-exist (translate-logical-pathname dir))
	(if (not (cl-fad:directory-exists-p dir))
	    (sb-posix:mkdir dir #o777))
	t))
  #-(or sbcl)
  (error "make-directory not implemented"))
