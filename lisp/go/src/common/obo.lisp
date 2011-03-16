;;;;
;;;; Chunk-parse OBO files.
;;;;
;;;; WARNING: direct operations on large OBO sets can cause
;;;; emacs/slime to throw a complete fit. Try wrapping operations that
;;;; would cause large things to print into forms that protect the
;;;; top-level. E.g.:
;;;;
;;;;  (progn (setf o (d2d:marshal "/tmp/obo")) t)
;;;;  (progn (setf o1 (trial1 o)) t)
;;;;

;; ...
(require :cl-ppcre)

;; ...
(defpackage :obo
  (:use
   :cl))
  ;; (:export 
  ;;  :parse-file))

(in-package :obo)

;;;
;;; Dumb parsing.
;;;
      
(defparameter +term-header-re+ (cl-ppcre:create-scanner "\\[Term\\]"))

(defun double-return-split (string)
  (cl-ppcre:split "\\n\\n" string))

(defun single-return-split (string)
  (cl-ppcre:split "\\n" string))

(defun colon-space-to-cons (string)
  ;; "\"foo: bar\" -> '(:foo . \"bar\")"
  (let ((vals (cl-ppcre:split "\:\\s" string)))
    (cons (intern (string-upcase (elt vals 0)) :keyword)
	  (elt vals 1))))

(defun record-to-cons (record)
  (let ((info (cdr record)))
    (mapcar #'(lambda (x)
		(colon-space-to-cons x))
	    info)))

(defun parse-file (file-loc)
  (mapcar #'record-to-cons
	  (remove-if #'(lambda (x) 
			 (or (not (cl-ppcre:scan +term-header-re+ (car x)))
			     (string-equal (car x) "")))
		     (mapcar #'single-return-split
			     (double-return-split (d2d:file->string file-loc))))))

;;;
;;; Filters and filtering.
;;;

(defun stanza-test-positive (stanza label str)
  (loop for line in stanza
     thereis (and (eq (car line) label)
		  (string= (cdr line) str))))

(defun stanza-test-negative (stanza label str)
  (loop for line in stanza
     never (and (eq (car line) label)
		(string= (cdr line) str))))

(defun remove-obsolete (parsed-obo-file &key (test nil))
  (let ((result (remove-if #'(lambda (x)
			       (stanza-test-positive x :IS_OBSOLETE "true"))
			   parsed-obo-file)))
    (if test (length result) result)))

(defun only-aspect (parsed-obo-file aspect &key (test nil))
  (let ((result (remove-if #'(lambda (x)
			       (stanza-test-negative x :NAMESPACE aspect))
			   parsed-obo-file)))
    (if test (length result) result)))

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

