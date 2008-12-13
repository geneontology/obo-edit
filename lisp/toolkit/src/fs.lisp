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
	   :cd))
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
(defun cd (&optional (path "/home/sjcarbon"))

  #+(or sbcl)
  ;;(if (path-supplied-p)
  (setf *default-pathname-defaults* (truename path))
  ;;    (setf *default-pathname-defaults* (truename path)))

  #-(or sbcl)
  (error "get-cwd not implemented"))
