;;;; -*- mode: Lisp -*-
;;;;
;;;; A simple class for logging. Trivial and high overhead. At some
;;;; point: move on to something cuter, like the mailboxes:
;;;; http://cl-cookbook.sourceforge.net/process.html
;;;;

(defpackage :bb-log
  (:use :cl)
  (:export
   :simple-log
   :kvetch))
(in-package :bb-log)


(defclass simple-log ()
  ((log-out
    :accessor log-out
    :initform nil
    :initarg :log-out)))

(defgeneric kvetch (log obj)
  (:documentation "..."))

;; TODO/BUG: I'm not sure how smart this will be with multiple
;; threads...
(defmethod kvetch ((log simple-log) obj)
  "Can log object to STDOUT, file (as string), or stream."
  (let ((out (log-out log)))
    (cond
      ((null out)
       (format nil "\[~A\] ~A" (bb-time:humanstamp) obj))
      ((stringp out)
       (with-open-file (lstream out
				:direction :output
				:if-exists :append
				:if-does-not-exist :create)
	 (format lstream "\[~A\] ~A~%" (bb-time:humanstamp) obj)))
      (t
       (format out "\[~A\] ~A" (bb-time:humanstamp) obj)))))
