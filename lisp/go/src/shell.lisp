;;;;
;;;; TODO: redo with engine instead of database.
;;;;

;;(clc:clc-require :go-database)
(clc:clc-require :go-engine)
(clc:clc-require :go-model)
(defpackage :go-shell
  (:use :cl
	:go-engine
	:go-model)
	;;:go-engine)
  (:nicknames :gosh))
;;   (:export :meta))
(in-package :go-shell)

;;;
;;; Some variables for connection.
;;;

(defvar *go-database-name* "go_latest_lite"
  "Database name for GO database.")
(defvar *go-database-host* "localhost"
  "Host for GO database.")
(defvar *go-database-user* ""
  "User for GO database.")
(defvar *go-database-password* ""
  "Password for GO database.")

(defvar *go-database* nil
  "Holder for GO database connection.")

(defvar *current-term* nil
  "The current term under consideration.")

(defvar *current-gene-product* nil
  "The current gene product under consideration.")

;;
(defvar *mark-list* nil
  "A list of marked terms for later use.")

;;
(defun connect (&key
		(host *go-database-host*)
		(name *go-database-name*)
		(user *go-database-user*)
		(password *go-database-password*))
  "Connect/reconnect to the database."
  (when *go-database*
    (close-db *go-database*))
  (setf *go-database* (make-instance 'go-db)) 
  (open-db *go-database*
	   :dbname name
	   :dbhost host
	   :user user
	   :password password))

;; TODO: disconnect...
(defun disconnect ()
  "Disconnect from the database."
  (when *go-database*
    (close-db *go-database*)))

(defun term? (acc)
  "Get information about a term."
  (go-engine:get-term *go-database* acc))

(defun meta? ()
  "Get information about the database."
  (go-engine:get-instance-data *go-database*))

;;;
;;; Mark handling.
;;;

(defun mark (items-to-mark)
  "Add items from the marked list."
  (setf *mark-list*
	(append *mark-list* (listify items-to-mark))))

(defun marked ()
  "Return a list of marked items."
  *mark-list*)

 (defun unmark (items-to-unmark)
   "Remove items from the marked list."
   (setf *mark-list*
	 (remove-list-from-list (listify items-to-unmark) *mark-list*)))

;; TODO: this could be moade a little more flexible.
(defun remove-list-from-list (removal-list base-list)
  "Remove the contents of one list from another."
  (if (not removal-list)
      base-list
    (remove-list-from-list
     (cdr removal-list)
     (remove-if #'(lambda (x)
		    (equalp (car removal-list) x))
		base-list))))

(defun listify (thing)
  "Make the argument a list."
  (if (not (listp thing)) (list thing) thing))
