;;;;
;;;; JavaScript emitter for some of the files in AmiGO.
;;;;
;;;; NOTE: currently standalone--not part of the go package.
;;;;

;;(require :toolkit)
(require :cl-ppcre)
(require :parenscript)
;;(require :amigo-config)

(defpackage :amigo-js
  (:use :cl
	:parenscript))
(in-package :amigo-js)


;;
(setf parenscript:*js-string-delimiter* #\")


;;;
;;; General utilities.
;;;


(defun kvetch (obj)
  (format t "~A~%" obj))

(defun join-strings (list &key (with ""))
  "Concatenate a list of strings."
  ;;(format nil "~{~A~^~}" list joiner))
  (format nil (concatenate 'string "~{~A~^" with "~}") list))

(defun split-string (string &key (with "\\s+"))
  "Decompose a JS namespace string into a list of strings."
  (cl-ppcre:split with string))

(defun flatten (xlist)
  "Flatten a tree into a list."
  (labels ((rec (xlist acc)
             (cond ((null xlist) acc)
                   ((atom xlist) (cons xlist acc))
                   (t (rec (car xlist) (rec (cdr xlist) acc))))))
    (rec xlist nil)))


;;;
;;; Task specific utilities.
;;;


(defun build-ns-list (list)
  "Creates a list of all necessary namespaces for a given namespace string."
  (labels ((rec (list acc)
	     (cond
	       ((null list) acc)
	       (t
		(let ((item (list (car list))))
		  (rec (cdr list) (append (mapcar #'(lambda (x)
						      (append x item))
						  acc)
					  (list item))))))))
    (reverse (mapcar #'reverse (rec (reverse list) nil)))))

(defun explode-namespace (ns-string)
  (mapcar #'(lambda (x) (join-strings x :with "."))
	  (build-ns-list (split-string ns-string :with "\\."))))

(defun exhaust-namespaces (namespaces)
  (remove-duplicates
   (flatten (mapcar #'explode-namespace namespaces))
   :test #'equal :from-end t))

;;;
;;; Add some macros to the environment.
;;;


;; (defpsmacro unroll-internal (x)
;;   `(defvar ,x  (slot-value robj 'results ',x)))

;; (defpsmacro unroll-internal-meta (x)
;;   `(defvar ,x  (slot-value robj 'results 'meta ',x)))

;; (defpsmacro unroll-funcall (x)
;;   `(setf (slot-value this ',x) (lambda () (return ,x))))


;;;
;;; Emitters.
;;;

(defun js-header ()
  (format nil  "////~%//// This was automatically generated using parenscript.~%////"))

(defun foo ()
  (let ((ns 'org.bbop))
      (ps
	(if (== (typeof (lisp ns)) "undefined")
	    (throw (+ "No module: " (lisp ns)))))))

(defun js-namespace-check (ns)
  (join-strings
   (mapcar #'(lambda (ns)
	       (ps
		 (if (== (typeof (lisp ns)) "undefined")
		     (throw (+ "No module: " (lisp ns))))))
	   (exhaust-namespaces ns))
   :with "~%"))

(defun js-namespace-build (ns)
  (join-strings
   (mapcar #'(lambda (ns)
	       (ps
		 (if (== (typeof (lisp ns)) "undefined")
		     (setf (lisp ns) (create)))))
	   (exhaust-namespaces ns))
   :with "~%"))

(defun js-getter-setters (prop-list)
  (join-strings
   (mapcar #'(lambda (prop)
	       (ps
		 ;;(defvar (lisp prop) (slot-value robj 'results (lisp prop)))
		 ;;(defvar prop (slot-value robj 'results (lisp prop)))
		 (setf (slot-value this (lisp prop))
		       ;;(lambda () (return (lisp prop))))))
		       (lambda ()
			 (return (slot-value robj 'results (lisp prop)))))))
	   prop-list)
   :with "~%"))
  
(defun js-body (namespace properties)
  "..."
  (ps
    ;;(setf (lisp namespace)
    (setf (slot-value foo 'bib 'bob)
	  (lambda (robj)
	    (lisp (js-getter-setters properties))))))


;;;
;;;
;;;


;;
(defun amigo-term-js ()

  ;; Create term.js...
  (join-strings
   (list
    (js-header)
    (js-namespace-check '("org.bbop.amigo"
			  "org.bbop.amigo.go_meta"))
    (js-namespace-build '("org.bbop.amigo.term"))
    (js-body "org.bbop.amigo.term"
	     '("terms")))
   :with "~%"))
