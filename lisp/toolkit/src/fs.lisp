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


(defpackage :toolkit-fs
  (:use :cl
	:cl-fad)
  (:export :pwd
	   :cd
	   :list-files
	   :list-directories))
(in-package :toolkit-fs)

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
  (toolkit-conv:filter (cl-fad:list-directory dir)
		       #'(lambda (x) (cl-fad:directory-exists-p x))))

;;
(defun list-directories (dir)
  "List all of the directories in directory <dir>."
  (toolkit-conv:filter (cl-fad:list-directory dir)
		       #'(lambda (x) (not (cl-fad:directory-exists-p x)))))
