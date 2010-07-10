;;;; -*- mode: Lisp -*-
;;;;
;;;; NOTE: Backed this with local-time.
;;;;

(defpackage :bb-time
  (:use :cl
	:local-time)
  (:export
   :date-string
   :timestamp
   :humanstamp
   ;; TODO: timestamp-diff
   
   :timer
   :seconds))
(in-package :bb-time)

;; Example usage: (tdb-time-stamp (get-universal-time))
;; Example usage: (tdb-time-stamp)
(defun timestamp (&optional (utime (timestamp-to-universal (now))))
  "Return a string representing universal time."
  utime)

;; TODO: unneeded?
;; Example:
;;    (date-string (local-time:timestamp-to-universal (local-time:now)))
(defun date-string (&optional (utime (timestamp)))
  "Return a date string representing universal time. Example
usage: (date-string (get-universal-time))."
  (let ((day-names '("Monday" "Tuesday" "Wednesday" "Thursday"
                     "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time utime)
      (declare (ignore dst-p))
      (format nil "~2,'0d:~2,'0d:~2,'0d on ~a, ~d/~2,'0d/~d (GMT~@d)"
              hour
              minute
              second
              (nth day-of-week day-names)
              month
              date
              year
              (- tz)))))

;; Example usage: (tdb-time-stamp (get-universal-time))
;; Example usage: (tdb-time-stamp)
(defun humanstamp (&optional (utime (timestamp)))
  "Return a date integer representing the inputted utime."
  (if utime
      (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
	  (decode-universal-time utime)
	(declare (ignore tz dst-p day-of-week))
	;; Sorry for let--don't want to return two values here.
	(let ((retval (parse-integer
		       (format nil "~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
			       year
			       month
			       date
			       hour
			       minute
			       second))))
	  retval)
	0)))

;;;

(defclass timer ()
  ((start-time
    :accessor start-time
    :initform (now)
    :initarg :start-time)
   (last-time
    :accessor last-time
    :initform 0)))

(defgeneric seconds (in-timer)
  (:documentation "..."))

(defmethod seconds ((in-timer timer))
  (let ((then (start-time in-timer)))
    (- (timestamp-to-universal (now))
       (timestamp-to-universal then))))

  ;; (let ((now (get-universal-time)))
    ;; (- now (start-time in-timer))))
