;;;; -*- mode: Lisp -*-
;;;;
;;;; High-level database handling--think gross things like making
;;;; tables and the like. Finer things will be handled elsewhere.
;;;;
;;;; TODO: finish set-connection
;;;;

(defpackage :tanuki-db
  (:use :cl
	:postmodern
	:tanuki-schema)
  (:export
   :*connection-parameters*
   :set-connection
   :connect-repl
   :disconnect-repl
   :create-database
   :clear-database
   :table-count
   :table-dump
   :reset-database
   ))
(in-package :tanuki-db)

;;;
;;; Schema.
;;;

(defparameter *connection-parameters*
  '("tanuki" "tanuki_user" "tanuki_pass" "localhost")
  "For Postgres, a list of: dbname, dbuser, dbpass, and server")

;;;
;;; Connection and creation.
;;;

;; TODO:
(defun set-connection ()
  "TODO: Change the embedded connection parameters."
  nil)
  
;;
(defun connect-repl ()
  "Make sure that we have default connection for the repl. All permissions,
etc., have to be taken care of first."
  (if (or (null *database*) (not (connected-p *database*)))
      (progn
	(apply #'connect-toplevel *connection-parameters*)
	(connected-p *database*))))

(defun disconnect-repl ()
  "Disconnect the top-level databse from the repl."
  (disconnect-toplevel))

(defun create-database ()
  (dolist (table *tables*)
    (when (not (table-exists-p table))
      (execute (dao-table-definition table))))
  (dolist (seq *sequences*)
    (when (not (sequence-exists-p seq))
      (execute (:create-sequence seq)))))

(defun clear-database ()
  "Drop all tables and sequences from the database."
  (dolist (table (list-tables))
    (when (table-exists-p table)
      (execute (:drop-table table))))
  (dolist (seq (list-sequences))
    (when (sequence-exists-p seq)
      (execute (:drop-sequence seq)))))

;;;
;;; Querying.
;;;

(defun table-count (table-symbol)
  "Get a count of rows in a table."
  (query (:select (:count '*) :from table-symbol) :single))

(defun table-plist (table-symbol)
  "Return all rows in a table as a plist."
  (query (:select '* :from table-symbol) :plists))

;;;
;;; Reseting.
;;;

(defun reset-database (url-str data-str)
  "Wipe and rebuild the database (or get the system into a usable
state from nothing). After running this, agents should be able to have
free reign--the meta table is populated and the first (target/start)
page is defined (with no hits)."
  (connect-repl)
  (clear-database)
  (create-database)
  (let* ((first-page-id (sequence-next 'page-id-seq))
	 (new-meta (make-instance 'meta 
				  :start (bb-time:timestamp)
				  :target url-str
				  :data data-str))
	 (first-page (make-instance 'page
				    :id first-page-id
				    :url url-str
				    :internal 1))
	 (first-aset (make-instance 'argument-set
				    :id (sequence-next 'argument-set-id-seq)
				    :page-id first-page-id
				    :raw-url url-str
				    :unique-url url-str
				    :todo 1
				    ;;:request-method ""
				    ;;:request-type "base"
				    )))
    (insert-dao new-meta)
    (insert-dao first-page)
    (insert-dao first-aset)
    (values (start new-meta)
	    (target new-meta)
	    (data new-meta)
	    (id first-page)
	    (id first-aset))))
