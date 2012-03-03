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
  (add-graph-static "render/phylo.js")
  (add-graph-static "render/test1.js")
  (add-external-static "raphael-min.js")
  (add-external-static "graffle.js")
  (hunchentoot:define-easy-handler (main-test-page :uri "/test.html") ()
    (setf (hunchentoot:content-type*) "text/html")
    (test-html))
  (hunchentoot:define-easy-handler (main-graffle-page :uri "/graph.html") ()
    (setf (hunchentoot:content-type*) "text/html")
    (graffle-html))
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
	    (:script :type "text/javascript" :src (ccat +base+ "raphael-min.js"))
	    (:script :type "text/javascript" :src (ccat +base+ "render/phylo.js"))
	    (:script :type "text/javascript" :src (ccat +base+ "render/test1.js")))
	   (:body
	    (:div :id "test1")
	    (:div :id "test2" :style "width: 500px;")
	    (:div :id "test3")))))

;; Take a look at the online demo in a closed environment.
(defun graffle-html ()
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html lang=\"en\">
    <head>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
        <title>Test</title>
        <script src=\"http://localhost:4242/core.js\" type=\"text/javascript\" charset=\"utf-8\"></script>
        <script src=\"http://localhost:4242/model.js\" type=\"text/javascript\" charset=\"utf-8\"></script>
        <script src=\"http://localhost:4242/tree.js\" type=\"text/javascript\" charset=\"utf-8\"></script>
        <script src=\"http://localhost:4242/raphael-min.js\" type=\"text/javascript\" charset=\"utf-8\"></script>
        <script src=\"http://localhost:4242/render/phylo.js\" type=\"text/javascript\" charset=\"utf-8\"></script>
        <script src=\"http://localhost:4242/render/test1.js\" type=\"text/javascript\" charset=\"utf-8\"></script>

        <style type=\"text/css\" media=\"screen\">
            \#holder \{
                -moz-border-radius: 10px;
                -webkit-border-radius: 10px;
                border: solid 1px \#333;
    height: 480px;
    left: 50%;
    margin: -240px 0 0 -320px;
    position: absolute;
    top: 50%;
    width: 640px;

            \}
        </style>
    </head>
    <body>
        <div id=\"holder\"></div>
    </body>
</html>")