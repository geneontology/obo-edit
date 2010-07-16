;;;; -*- mode: Lisp -*-
;;;;
;;;; Test web agent.

(defpackage :mechanize-test
  (:use
   :cl
   :fiveam
   :mechanize)
  (:export
   ))
(in-package :mechanize-test)

;; From cl-user snippet.
(defun read-file-as-string (path)
  "Whole file as string."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

;;;
;;; Testing.
;;; TODO: Better testing of everything...
;;; TODO: Flesh this all out.
;;;

(defparameter +test-server+ nil)
(defparameter +test-url+ "http://localhost:4567/test1")
(defparameter +test-file+ "/home/sjcarbon/local/src/svn/geneontology/lisp/mechanize/t/test-links.html")

(defun start-test-server ()
  (stop-test-server)
  (setf +test-server+ (make-instance 'hunchentoot:acceptor :port 4567))
  (handler-case
      (hunchentoot:start +test-server+)
    (HUNCHENTOOT::HUNCHENTOOT-SIMPLE-ERROR (hse) (declare (ignore hse)) nil)))

(defun stop-test-server ()
  (when +test-server+
    (handler-case
	(hunchentoot:stop +test-server+)1
      (SIMPLE-ERROR (se) (declare (ignore se)) nil))
    (setf +test-server+ nil)))

(hunchentoot:define-easy-handler (test-1 :uri "/test1") ()
  (setf (hunchentoot:content-type*) "text/html")
  (format nil "~A" (read-file-as-string +test-file+)))

;; (test link-1
;;   "Test the links in out example document \"test-links.html\"."
;;   (let ((
  


;; (defun test-extract-links ()
;;   (let ((doc "<p><a href = \"/foo\">blah</a></p>"))
;;     (extract-links doc)))
