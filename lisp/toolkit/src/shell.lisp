;;;;
;;;; Shell
;;;;


(defpackage :toolkit-shell
  (:use :cl)
  (:export :getenv
	   :cline))
(in-package :toolkit-shell)

;; From the CL cookbook.
(defun getenv (name &optional default)
  ""
  #+CMU
  (let ((x (assoc name ext:*environment-list* :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

;; Taken mostly from the CL cookbook.
(defun cline (&optional pos)
  ""
  (if pos
      (or ; return whole list
       #+SBCL (nth (1+ pos) (cdr sb-unix::*posix-argv*))
       #+LISPWORKS system:*line-arguments-list*
       #+CMU extensions:*command-line-words*
       nil)
      (or ; return whole list
       #+SBCL (cdr sb-unix::*posix-argv*)
       #+LISPWORKS system:*line-arguments-list*
       #+CMU extensions:*command-line-words*
       nil)))
