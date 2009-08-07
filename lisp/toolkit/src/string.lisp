;;;;
;;;; Common string utilities and tricks...
;;;;

(defpackage :toolkit-string
  (:use :cl)
  (:export :date-string
	   :ccat))
(in-package :toolkit-string)


(defmacro ccat (&rest strings)
  "Concatenate a set of strings."
  `(concatenate 'string ,@strings))

(defun date-string (utime)
  "Return a date string representing universal time."
  (let ((day-names '("Monday" "Tuesday" "Wednesday" "Thursday"
		     "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
	(decode-universal-time utime)
      (format nil "~2,'0d:~2,'0d:~2,'0d on ~a, ~d/~2,'0d/~d (GMT~@d)"
	      hour
	      minute
	      second
	      (nth day-of-week day-names)
	      month
	      date
	      year
	      (- tz)))))
