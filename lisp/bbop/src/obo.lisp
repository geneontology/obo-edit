;;;;
;;;; Chunk-parse OBO files.
;;;;
;;;; NOTE: currently standalone--not part of the go package.
;;;;

;;(require :toolkit)
(require :cl-ppcre)

(defpackage :bbop-obo
  (:use
   :cl
   :bbop-utils)
  (:export 
   :parse-file))
(in-package :bbop-obo)

;;;

(defparameter +term-header-re+ (cl-ppcre:create-scanner "\\[Term\\]"))
;;(defparameter +term-header-re+ (cl-ppcre:create-scanner "\\[Term\\]"))

;;(defparameter *parse-hash* (make-hash-table))

;;;
;;;
;;;
      
(defun double-return-split (string)
  (cl-ppcre:split "\\n\\n" string))

(defun single-return-split (string)
  (cl-ppcre:split "\\n" string))

(defun colon-space-to-cons (string)
;;  "\"foo: bar\" -> '(:foo . \"bar\")"
  (let ((vals (cl-ppcre:split "\:\\s" string)))
    (cons (intern (string-upcase (elt vals 0)) :keyword)
	  (elt vals 1))))

(defun parse-record (record)
  (let ((info (cdr record)))
    (mapcar #'(lambda (x)
		(colon-space-to-cons x))
	    info)))

(defun parse-file (file-loc)
  (mapcar #'parse-record
	  (remove-if #'(lambda (x) 
			 (or (not (cl-ppcre:scan +term-header-re+ (car x)))
			     (string-equal (car x) "")))
		     (mapcar #'single-return-split
			     (double-return-split (file-string file-loc))))))

;; (defparameter +record+ nil)
;; (defun parse (file-loc)
;;   (let ((parsed (file-to-term-sexps file-loc)))
;;     (car parsed)))
;;     (loop for record in parsed
;;        do (let ((info (cdr record))) ; dump [Term] header
;; 	    (loop for line in record
;; 	       do (progn
		    
;; 	      )))))
;;  t)

;; ;; BUG: yes, this clobbers things so there are single values where
;; ;; there should be double--remember, this is just trying to get enough
;; ;; to look at couchdb...
;; (defun parse-record-to-plist (record)
;;   (let ((info (cdr record)))
;;     (flatten (mapcar #'(lambda (x)
;; 			 (let ((two-vals (colon-space-split x)))
;; 			   (cons (intern (string-upcase (elt two-vals 0)))
;; 				 (elt two-vals 1))))
;; 		     info))))

