;;;;
;;;; Not in ASD for Tanuki yet; usage:
;;;;  (load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki-web.lisp")
;;;;
;;;; This will have to build on top of tanuki-store and tanuki-agent.
;;;;
;;;; HAMMER TIME:
;;;;    ab -c 10 -n 1000 http://127.0.0.1/
;;;;

;;(clc:clc-require :aserve)
;;(clc:clc-require :webactions)
(clc:clc-require :hunchentoot)
(clc:clc-require :parenscript)
(defpackage :tanuki-web (:use :cl
			      :hunchentoot
			      ;;:net.aserve
			      ;;:net.html.generator
			      :parenscript))
(in-package :tanuki-web)


(defvar +port+ 8888 "Default web server port.")
(defparameter +server+ nil "")

;;;
;;; Pages.
;;;

(defun tanuki-js (req ent)
  "JS code for TWI. Returns as a string."
  (declare (ignore req ent))
  (ps
    (defun tanuki-start ()
      (alert "START"))
    (defun tanuki-stop ()
      (alert "STOP"))
    (defun tanuki-status ()
      (alert "STATUS"))))

(defun main-page (req ent)
  "HTML code for TWI. Returns as a string."
  (declare (ignore req ent))
  (ps-html
   (:html
     (:head (:title "Tanuki Manager")
	    ((:script :language "JavaScript" :src "/js/tanuki.js")))
     (:body (:h1 "Tanuki Manager")
	    (:p ((:a :href "#"
		     :onclick (ps-inline (tanuki-start)))
		 "Start") " rummaging.")
	    (:p ((:a :href "#"
		     :onclick (ps-inline (tanuki-stop)))
		 "Stop") " rummaging.")
	    (:p ((:a :href "#"
		     :onclick (ps-inline (tanuki-status)))
		 "Get") " the current status.")))))

;;;
;;; Controls.
;;;

(defmacro webify (&key
		  (path "/")
		  (page #'(lambda (req ent) "empty page"))
		  (type "html"))
  "TODO: documentation."
  (let ((type-string (if (string= "js" type)
			 "application/javascript; charset=UTF-8"
			 "text/html; charset=UTF-8")))
    `(publish :path ,path
	      :content-type ,type-string
	      :function
	      #'(lambda (req ent)
		  (with-http-response (req ent)
		    (with-http-body (req ent)
		      (princ (funcall ,page req ent) *html-stream*)))))))

(defun web-up ()
  "Bring up the tanuki web interface."

  ;; TODO: bring up tanuki.

  ;; Start server.
  (setf +server+ (start-server :port +port+)))

;;   ;; Publish pages.
;;   (webify :path "/empty")
;;   (webify :path "/" :page #'main-page)
;;   (webify :path "/js/tanuki.js" :page #'tanuki-js :type js)

;;  (publish :path "/"
;;	   :content-type "text/html; charset=UTF-8"
;;	   :function
;;	   #'(lambda (req ent)
;;	       (with-http-response (req ent)
;;		 (with-http-body (req ent)
;;		   (princ (main-page req ent) *html-stream*)))))

;;   (publish :path "/hello"
;; 	   :content-type "text/html"
;; 	   :function #'(lambda (req ent)
;; 			 (with-http-response (req ent)
;; 			   (with-http-body (req ent)
;;                              (html "Hello, World!")))))

;;   (publish-file :path "/static"
;; 		:file "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/url_examples.txt"))

(defun web-down ()
  "Stop the tanuki web interface."
  ;; TODO: take down tanuki.
  (if +server+ (stop-server +server+)))
