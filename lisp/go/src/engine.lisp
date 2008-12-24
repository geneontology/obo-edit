;;;;
;;;; The convenience layer over the schema to keep us sane...  The
;;;; engine is pretty much the same as a GO DB object, but has some
;;;; stuff attached to make the model more usable--think GO::AppHandle
;;;;
;;;; The stuff in here should mostly be things to help prop-up the
;;;; higher-level go-model (which should do things like graphs and
;;;; cursors).
;;;;
;;;; Should just return raw schema objects and data.
;;;;

(clc:clc-require :clsql)
(clc:clc-require :clsql-mysql)
(defpackage :go-engine
  (:use :cl
	:clsql-mysql
	:clsql
	:go-schema)
  (:export :engine
	   :start-engine
	   :stop-engine
	   :engine-status
	   :get-term-by-acc
	   :get-term-by-name
	   :get-relationship-by-accs
	   :get-count
	   :get-instance-data))
(in-package :go-engine)

;;;
;;; Add more slots later?
;;;

(defclass engine ()
  ((database
    :initform nil
    :accessor database
    :documentation "All the info will be in the DB.")))

;;;
;;; GO DB dis/connect and status.
;;;

;; TODO: have some package variables to set these...
(defmethod start-engine ((e engine)
			 &key
			 (dbname "go_latest_lite")
			 (dbhost "localhost")
			 (user "")
			 (password ""))
  ""
  (with-slots
   (database) e
   (setf database (connect (list dbhost dbname user password)
			   :database-type :mysql
			   :if-exists :warn-old))))

(defmethod stop-engine ((e engine))
  "Close the connection to the database."
  (with-slots
   (database) e
   (if database
       (disconnect :database database))))

;; This largely still needs to be done.
(defmethod engine-status ((e engine))
  "Get the status of the DB."
  (with-slots
   (database) e
   (if database
       (status :database database))))

;;;
;;; Fundamental data grabbing.
;;;

(defmethod get-instance-data ((e engine))
  ""
  #.(locally-enable-sql-reader-syntax)
  (let ((instance-data
	 (with-slots
	  (database) e
	  (car (select [*] :from [instance-data] :database database)))))
    (list :date (nth 0 instance-data)
	  :type (nth 1 instance-data)
	  :comment (nth 2 instance-data)))
  #.(restore-sql-reader-syntax-state))

(defmethod get-count ((e engine) table-symbol)
  "Get the count off of different tables."
  #.(locally-enable-sql-reader-syntax)
  (with-slots
   (database) e
   (car (select [count [*]] :database database :from table-symbol :flatp t)))
  #.(restore-sql-reader-syntax-state))

(defmethod get-term-by-acc ((e engine) acc)
  ""
  #.(locally-enable-sql-reader-syntax)
  (caar
   (with-slots
    (database) e
    (select 'term
	    :database database
	    :where [= [term.acc] acc]
	    :caching nil)))
  #.(restore-sql-reader-syntax-state))

(defmethod get-term-by-name ((e engine) name)
  ""
  #.(locally-enable-sql-reader-syntax)
  (mapcar #'car
	  (with-slots
	   (database) e
	   (select 'term
		   :database database
	    :where [like [term.name] name]
	    :caching nil)))
  #.(restore-sql-reader-syntax-state))

(defmethod get-relationship-by-accs ((e engine) subject-acc object-acc)
  ""
  #.(locally-enable-sql-reader-syntax)
  (let ((subject-term-id (slot-value (get-term-by-acc e subject-acc)
				     'go-schema::id))
	(object-term-id (slot-value (get-term-by-acc e object-acc)
				    'go-schema::id)))
    (with-slots
     (database) e
     (caar
      (select 'term2term
	      :database database
	      :where [and [= [term2term.term2_id] subject-term-id]
	                  [= [term2term.term1_id] object-term-id]]
	      :caching nil))))
  #.(restore-sql-reader-syntax-state))
