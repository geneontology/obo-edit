;;;;
;;;; Bootstrapper for testing.
;;;;

(ql:quickload "hunchentoot")
(ql:quickload "cl-who")
(defpackage :test
  (:use :cl
	:cl-who))
(in-package :test)

;;;
;;; Server settings.
;;;

(defvar *server* nil)
(defparameter +logs+ "/tmp/")
(defparameter +root+ "/home/sjcarbon/local/src/svn/geneontology/")
(defparameter +base+ "http://localhost:4242/")

(defmacro ccat (&rest strings) `(concatenate 'string ,@strings))

(setf hunchentoot::*message-log-pathname* (ccat +logs+ "comb.log"))
;; (setf hunchentoot::*access-log-pathname* +logs+)
(setf hunchentoot::*show-lisp-errors-p* t)
;;(setf hunchentoot::*attribute-quote-char* #\")

;;;
;;; Server controls.
;;;

(defun add-static (http-name fs-name)
  (push (hunchentoot:create-static-file-dispatcher-and-handler
	 http-name fs-name) hunchentoot:*dispatch-table*))
  
(defun add-graph-static (fname)
  (add-static (ccat "/" fname) (ccat +root+ "javascript/graph/" fname)))
  
(defun add-external-static (fname)
  (add-static (ccat "/" fname) (ccat +root+ "javascript/external/" fname)))
  
(defun start ()
  (add-graph-static "core.js")
  (add-graph-static "model.js")
  (add-graph-static "tree.js")
  (add-external-static "raphael-min.js")
  (hunchentoot:define-easy-handler (main-test-page :uri "/test.html") ()
    (setf (hunchentoot:content-type*) "text/html")
    (test-html))
  ;; (hunchentoot:define-easy-handler (what-is-this-2 :uri "/godot.js") ()
  ;;   (setf (hunchentoot:content-type*) "application/javascript")
  ;;   (godot-js))
  (setf *server* (make-instance 'hunchentoot:acceptor :port 4242))
  (hunchentoot:start *server*))

(defun stop ()
  (hunchentoot:stop *server*))

;;;
;;; Page writers.
;;;

;; BUG: why doesn't this work?
(defun js-inc (name)
  (cl-who:with-html-output-to-string (ostrm)
    (:script :type "text/javascript" :src (ccat +base+ name))))
  
(defun test-html ()
  (cl-who:with-html-output-to-string (strm)
    (:html (:head
	    ;; (js-inc "core.js")
	    ;; (js-inc "model.js")
	    ;; (js-inc "tree.js")
	    ;; (js-inc "raphael-min.js"))
	    (:script :type "text/javascript" :src (ccat +base+ "core.js"))
	    (:script :type "text/javascript" :src (ccat +base+ "model.js"))
	    (:script :type "text/javascript" :src (ccat +base+ "tree.js"))
	    (:script :type "text/javascript" :src (ccat +base+ "raphael-min.js")))
	   (:body
	    (:div :id "test1")))))

