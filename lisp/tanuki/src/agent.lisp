;;;;
;;;;
;;;;

;;(clc:clc-require :fiveam)
(defpackage :tanuki-agent
  (:use :cl
	:sb-ext)
  (:export ))
(in-package :tanuki-agent)

;;
(defun launch-sleepy-agent (alarm)
  ""
  (sb-thread:make-thread (lambda ()
			   (sleepy-agent alarm))
			 :name "sleepy-agent"))

(defun sleepy-agent (alarm)
  "Sleep agent will sleep until alarm evals to t."
  (loop
     (when alarm
       (format t "Saw alarm...")
       (return nil))
     (sleep 1)
     (format t "Zzz...")))
