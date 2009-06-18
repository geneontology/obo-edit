;;;;
;;;; 
;;;;

(require :cl-irc)
(require :cl-ppcre)
(require :toolkit)

(defpackage :gobot
  (:use :cl
	:cl-irc
	:ppcre
	:toolkit))
;;:go-model))

(in-package :gobot)

;;;
;;; 
;;;

(defvar *connection* nil "Server where GObot hangs out.")
(defvar *channel* nil "channel GObot hangs out.")
(defvar *self* "gobot" "channel GObot hangs out.")
(defvar *engine* nil "")


(defun ready ()

  (setf *connection* (connect :username *self*
			      :nickname *self*
			      ;;:server "irc.freenode.net"))
			      :server "localhost"))
  (setf *channel* "#AmiGO")
  (setf *engine* (make-instance 'go-engine::engine))
  (go-engine::start-engine *engine*)
  t)

(defun start ()
  (ready)
  (join *connection* *channel*)
  (add-hook *connection* 'irc::irc-privmsg-message 'process-message)
  (read-message-loop *connection*))
;;   (add-hook *connection* 'irc::irc-message 'foo)
;;   (add-hook *connection* 'irc::irc-message-event 'foo)
;;   (add-hook *connection* 'irc::irc-privmsg-message 'foo))

(defun stop ()
  (quit *connection* "GObot has gotten tired"))

;; source
(defun process-message (message)
  ""
  ;;   (format t "args: ~A~%" (arguments message))
  ;;   (format t "source: ~A~%" (source message))
  ;;   (format t "connection: ~A~%" (connection message))
  (let ((target (car (arguments message)))
	(text (cadr (arguments message))))
    (if (self-is-target-p target)
	 (private-response text)
	 (public-response text))))

(defun self-is-target-p (target-text)
  ""
  (if (string= (string-upcase *self*) (string-upcase target-text)) t nil))

(defun private-response (text)
  ""
  (format t "incoming private message: ~A~%" text)
  (let ((term-strings (all-matches-as-strings "GO\:\\d{7}" text)))
    (if term-strings
	(mapcar #'process-term-string term-strings))))

(defun process-term-string (gid-string)
  ""
  (let ((term-schema (go-engine::get-term-by-acc *engine* gid-string)))
    (privmsg *connection* *channel* (slot-value term-schema 'go-model::acc))
    (privmsg *connection* *channel* (slot-value term-schema 'go-model::name))))


(defun public-response (text)
  ""
  (format t "incoming public message: ~A~%" text)
  (privmsg *connection* *channel* "pub"))
