;;;; -*- mode: Lisp -*-
;;;;
;;;; High-level database handling--think gross things like making
;;;; tables and the like. Finer things will be handled elsewhere.
;;;;
;;;; TODO/BUG: Rather ickily using get-universal-time as the
;;;; timestamp. Take a look at something else (maybe simple-date?)
;;;; sometime...
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
   ;;:make-seed-page
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
    (if (not (table-exists-p table))
	(execute (dao-table-definition table))))
  (dolist (seq *sequences*)
    (if (not (sequence-exists-p seq))
	(execute (:create-sequence seq)))))

(defun clear-database ()
  "Drop all tables and sequences from the database."
  (dolist (table (list-tables))
    (execute (:drop-table table)))
  (dolist (seq (list-sequences))
    (execute (:drop-sequence seq))))

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

;; Seed the pages with the url from the meta target.
;; Internal function.
(defun make-seed-page ()
  "Internal function for clean"
  (if (= 0 (table-count 'page))
      (let ((seed-page (make-instance 'page
				      :id (sequence-next 'page-id-seq)
				      :url (query (:select 'target :from 'meta)
						  :single)
				      :internal 1)))
	(insert-dao seed-page)
	(id seed-page))))

(defun reset-database (url-str)
  "Wipe and rebuild the database (or get the system into a usable
state from nothing). After running this, agents should be able to have
free reign--the meta table is populated and the first (target/start)
page is defined (with no hits)."
  (connect-repl)
  (clear-database)
  (create-database)
  (let ((new-meta (make-instance 'meta 
				 ;;:start (bb-time:timestamp)
				 :start (get-universal-time)
				 :target url-str))
	(first-page (make-instance 'page
				   :id (sequence-next 'page-id-seq)
				   :url url-str
				   :internal 1)))
    (insert-dao new-meta)
    (insert-dao first-page)
    (values (start new-meta)
	    (target new-meta)
	    (id first-page))))

;; (defmethod get-page-from-url ((dbo tanuki-database) ustr)
;;   "Returns true if the URL is in the db, nil otherwise."
;;   (let ((result (sql-engine dbo :url ustr :visited nil)))
;;     (if result (car result) nil)))

;; (defmethod get-page-count ((dbo tanuki-database))
;;   "Return the number of pages."
;;   (sql-engine dbo :count t))

;; (defmethod get-unvisited-page-count ((dbo tanuki-database))
;;   "Return the number of unvisited pages."
;;   (sql-engine dbo :visited nil :count t))

;; (defmethod get-internal-page-count ((dbo tanuki-database))
;;   "Return the number of unvisited pages."
;;   (sql-engine dbo :internal t :count t))

;; (defmethod get-internal-unvisited-page-count ((dbo tanuki-database))
;;   "Return the number of unvisited pages."
;;   (sql-engine dbo :internal t :visited nil :count t))

;; (defmethod get-bad-page-count ((dbo tanuki-database))
;;   "Return the number of visited pages that failed."
;;   (sql-engine dbo :failed t :visited t :count t))

;; (defmethod get-odd-page-count ((dbo tanuki-database))
;;   "Return the number of visited pages that are odd."
;;   (sql-engine dbo :odd t :visited t :count t))

;; (defmethod get-nth-page ((dbo tanuki-database) n)
;;   "Return the nth PAGE in the database, or nil. Starts from 1."
;;   (let ((page-list (sql-engine dbo :limit 1 :offset (1- n))))
;;     (if page-list (car page-list)
;;       nil)))

;; (defmethod get-nth-unvisited-page ((dbo tanuki-database) n)
;;   "Return the nth unvisited PAGE in the database, or nil. Starts from 1."
;;   (let ((page-list (sql-engine dbo :limit 1 :offset (1- n) :visited nil)))
;;     (if page-list (car page-list)
;;       nil)))

;; (defmethod get-nth-internal-page ((dbo tanuki-database) n)
;;   "Return the nth internal PAGE in the database, or nil. Starts from 1."
;;   (let ((page-list (sql-engine dbo :limit 1 :offset (1- n) :internal t)))
;;     (if page-list (car page-list)
;;       nil)))

;; (defmethod get-nth-internal-unvisited-page ((dbo tanuki-database) n)
;;   "Return the nth internal unvisited PAGE in the database, or nil. Starts from 1."
;;   (let ((page-list (sql-engine dbo :limit 1 :offset (1- n) :visited nil :internal t)))
;;     (if page-list (car page-list)
;;       nil)))

;; (defmethod get-all-pages ((dbo tanuki-database))
;;   "Return a list of PAGES in the database (that meet a specified criteria)."
;;   (sql-engine dbo))

;; (defmethod get-all-internal-pages ((dbo tanuki-database))
;;   "Return a list of PAGES in the database (that meet a specified criteria)."
;;   (sql-engine dbo :internal t))

;; (defmethod get-visited-pages ((dbo tanuki-database))
;;   "Return a list of visited page URLs in the database."
;;   (sql-engine dbo :visited t))

;; ;; NOTE: we're using SQLite-isms to make the boolean stuff work.
;; (defmethod get-unvisited-pages ((dbo tanuki-database))
;;   "Return a list of unvisited page URLs in the database."
;;   (sql-engine dbo :visited nil))

;; (defmethod get-internal-unvisited-pages ((dbo tanuki-database))
;;   "Return a list of internal unvisited page URLs in the database."
;;   (sql-engine dbo :internal t :visited nil))
