;;;; -*- mode: Lisp -*-
;;;;
;;;; High-level database handling--think gross things like making
;;;; tables and the like. Finer things will be handled elsewhere.
;;;;

(defpackage :tanuki-db
  (:use :cl
	:postmodern
	:tanuki-schema)
  (:export
   ;; ...
   :make-tanuki-database
   :dbset
   :parameters
   :connect-repl
   :disconnect-repl
   :create-database
   :clear-database
   :reset-database
   ;; ...
   :row-count
   :table-plist
   ))
(in-package :tanuki-db)

;;;
;;; Schema.
;;;

(defclass tanuki-database ()
  ((dbname
    :accessor dbname
    :initform "tanuki"
    :initarg :dbname)
   (dbuser
    :accessor dbuser
    ;;:initform "tanuki_user"
    :initform ""
    :initarg :dbuser)
   (dbpass
    :accessor dbpass
    ;;:initform "tanuki_pass"
    :initform ""
    :initarg :dbuser)
   (dbhost
    :accessor dbhost
    :initform "localhost"
    :initarg :dbhost)))

(defun make-tanuki-database (&key (dbname nil) (dbuser nil)
				  (dbpass nil) (dbhost nil))
  "..."
  (let ((tdb (make-instance 'tanuki-database)))
    (when dbname (dbset tdb 'dbname dbname))
    (when dbuser (dbset tdb 'dbuser dbuser))
    (when dbpass (dbset tdb 'dbpass dbpass))
    (when dbhost (dbset tdb 'dbhost dbhost))
    tdb))

(defmethod parameters ((tdb tanuki-database))
  (list (dbname tdb) (dbuser tdb) (dbpass tdb) (dbhost tdb)))

(defgeneric dbset (db-obj sym val)
  (:documentation "..."))

(defmethod dbset ((tdb tanuki-database) sym val)
  (setf (slot-value tdb sym) val))

;;;
;;; Connection and creation.
;;;

;;
(defmethod connect-repl ((tdb tanuki-database))
  "Make sure that we have default connection for the repl. All permissions,
etc., have to be taken care of first."
  (with-connection
   (parameters tdb)
   (when (or (null *database*) (not (connected-p *database*)))
     (progn
       (apply #'connect-toplevel (parameters tdb))
       (connected-p *database*)))))

(defmethod disconnect-repl ((tdb tanuki-database))
  "Disconnect the top-level databse from the repl."
  (with-connection
   (parameters tdb)
   (disconnect-toplevel)))

(defmethod create-database ((tdb tanuki-database))
  "Get the most basic table structure into place."
  (with-connection
   (parameters tdb)
   (dolist (table *tables*)
     (when (not (table-exists-p table))
       (execute (dao-table-definition table))))
   (dolist (seq *sequences*)
     (when (not (sequence-exists-p seq))
       (execute (:create-sequence seq))))))
  
(defmethod clear-database ((tdb tanuki-database))
  "Drop all tables and sequences from the database (leave structure)."
  (with-connection
   (parameters tdb)
   (dolist (table (list-tables))
     (when (table-exists-p table)
       (execute (:drop-table table))))
   (dolist (seq (list-sequences))
     (when (sequence-exists-p seq)
       (execute (:drop-sequence seq))))))

(defmethod reset-database ((tdb tanuki-database) url-str data-str)
  "Wipe and rebuild the database (or get the system into a usable
state from nothing). After running this, agents should be able to have
free reign--the meta table is populated and the first (target/start)
page is defined (with no hits)."
  (with-connection
   (parameters tdb)
   ;;(connect-repl)
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
				     :clean-url url-str
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
	     (id first-aset)))))
