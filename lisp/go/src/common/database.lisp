;;;;
;;;; Handle DB connections, varibles, etc.
;;;;
;;;; WARNING/NOTE: Deprecated.
;;;;

(clc:clc-require :clsql)
(clc:clc-require :clsql-mysql)
(defpackage :go-database
  (:use :cl
	:clsql-mysql
  	:clsql)
  (:export :go-db
	   :open-db
	   :close-db
	   :db-status))
(in-package :go-database)

;;;
;;; A dummy class so we can juggle multiple DB connections
;;; easily. Actually, this may not be necessary in this application...
;;;

(defclass go-db ()
  ((database
    :initform nil
    :accessor database
    :documentation "All the info will be in the DB.")))

;;;
;;; DB Connections.
;;;

;;
(defmethod open-db ((dbo go-db)
		    &key
		    (dbname "go_latest_lite")
		    (dbhost "localhost")
		    (user "")
		    (password ""))
  ""
  (with-slots (database) dbo
    (setf database (connect (list dbhost dbname user password)
			    :database-type :mysql
			    :if-exists :warn-old))))

(defmethod close-db ((dbo go-db))
  "Close the connection to the database."
  (if dbo
      (with-slots (database) dbo
	(if database
	    (disconnect :database database)))))

;; This largely still needs to be done.
(defmethod db-status ((dbo go-db))
  "Get the status of the DB."
  (if dbo
      (with-slots (database) dbo
	(if database
	    (status :database database)))))
