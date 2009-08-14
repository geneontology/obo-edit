;;;;
;;;; 
;;;;

(defpackage :bbop-url
  (:use :cl
	:cl-ppcre
	:puri)
  (:export))
(in-package :bbop-url)


;; TODO put all into kappa...
(defclass bbop-url (uri)
  ((url :initform "http://localhost"
   :initarg :url)))

;; (defmethod canonical-string (uri)
;;   (


;(defmethod 


(defun create-bbop-url (url)
  "..."
  nil)

;; #<URI http://localhost/cgi-bin/amigo?mode=foo&bar=1&bar=2>
;; BBOP-URL> (uri-scheme foo)
;; :HTTP
;; BBOP-URL> (uri-scheme host)
;; ; Evaluation aborted.
;; BBOP-URL> (uri-host foo)
;; "localhost"
;; BBOP-URL> (uri-query foo)
;; "mode=foo&bar=1&bar=2"
;; BBOP-URL> (uri-path foo)
;; "/cgi-bin/amigo"

(defun create-url (url)
  (uri url))

(defun decompose-query (string)
  "Convert a URL query string into a cons list."
  (mapcar #'(lambda (x) (cons (car x) (cadr x)))
	  (mapcar #'(lambda (x) (cl-ppcre:split "=" x))
		  (cl-ppcre:split "&" string))))

(defun add-to-hash (key value hash)
  (when (not (gethash key hash))
    (setf (gethash key hash) nil

(defun uri-args (uri-obj)
  (let ((hash (make-hash-table))
	(qlist (uri-query uri-obj)))
    (mapcar #'(lambda (x)
		
		(setf (gethash (car x hash)
	    qlist)
    hash))
    

