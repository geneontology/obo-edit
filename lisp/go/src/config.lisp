;;;;
;;;; JavaScript emitter for some of the files in AmiGO.
;;;;
;;;; NOTE: currently standalone--not part of the go package.
;;;;

;;(require :toolkit)
(require :cl-ppcre)
(require :cl-json)
(require :cl-fad)

(defpackage :amigo-config
  (:use :cl))
(in-package :amigo-config)

;;;
(defparameter +file-loc+
  "/home/sjcarbon/local/src/cvs/go-dev/amigo/config.json"
  "Location of configuration file.")
(defparameter +config+
  nil
  "Structure containing information from parsed file. Initially nil.")

(defparameter *config-hash* (make-hash-table))


;;;
;;; General utility.
;;;

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

;;;
;;;
;;;


(defun parse-config (&optional (fname +file-loc+))
  "Information from the config file to a hash."
  (if (cl-fad:file-exists-p fname)
      (let ((jtext (file-string fname)))
	(mapcar #'(lambda (x)
		    (setf (gethash (car x) *config-hash*) (cdr x)))
		(json:decode-json-from-string jtext))
	t)))

(defun get-var (key)
  (let ((new-key
	 (intern (concatenate 'string "+" (symbol-name key) "+") "KEYWORD")))
    (gethash new-key *config-hash*)))
      
