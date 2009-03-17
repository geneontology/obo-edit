;;;;
;;;; Only really working towards SQLite3 so far...
;;;;

(require :clsql)
(require :clsql-sqlite3)
(require :cl-fad)
;;(require :clsql-mysql)
(defpackage :toolkit-simple-db
  (:use :cl
	:clsql
	;;:clsql-mysql
	:clsql-sqlite3))
(in-package :toolkit-simple-db)

;;
(defvar *default-database-type* :sqlite3
  "We'll be using SQLite3, oh yes...")
(defvar *default-database-directory* "/tmp"
  "The directory where we will store all of our databases.")

;; Happy SQLite3 CLSQL boilerplate...
(defun clsql/sqlite3-boilerplate ()
  "Use this to make the sqlite3 environment in clsql sane."
  (defmethod clsql-sys:database-get-type-specifier
      ((type (eql 'integer)) 
       args database 
       (db-type (eql :sqlite3)))
    (declare (ignore database db-type))
    (if args
	(format nil "INTEGER(~A)" (car args))
	"INTEGER"))
  (defmethod clsql-sys::database-pkey-constraint
      ((class 
	clsql-sys::standard-db-class) 
       (database 
	clsql-sqlite3::database))))

;;;
;;; Class and simple constructor.
;;;

(defclass simple-db (sqlite3-database)
  ((identiname :accessor name
	       :initarg :name)
   (identidir :accessor dir
	      :initarg :dir)
   (fullpath :accessor path
	     :initarg :path)))

(defun new-simple-db (name &optional (dir *default-database-directory*))
  "Fuzzy little constructor for simple-db."
  (make-instance 'simple-db
		 :name name
		 :dir dir
		 :path (merge-pathnames (toolkit-conv:ccat dir "/" name))))

;;

(defgeneric location-db (object)
  (:documentation "Where is my chizzit?."))

(defmethod location-db ((db simple-db))
  ""
  (toolkit-conv:ccat (dir db) "/" (name db)))



(defgeneric probe-db (object)
  (:documentation "See if the database <object> exists."))

(defmethod probe-db ((db simple-db))
  "This will mean checking for file existance for sqlite3."
  (cl-fad:file-exists-p (full-catter (dir db) (name db))))

(defgeneric create-db (object)
  (:documentation "Creates the DB associated with <object>."))

;; (defmethod create-db ((db simple-db))
;;   ""
;;   (clsql/sqlite3-boilerplate)

;;   ;; Create/connect to database.
;;   (let ((db-loc (full-catter (dir db) (name db))))
;;     (if (probe-db db-loc)
;;   (if (clsql:connect ((:database-type :sqlite3
;; 		      :if-exists :old
;; 		      :database db)) t nil))

(defgeneric connect-db (object)
  (:documentation "Connects to the DB associated with <object>."))

(defgeneric destroy-db (object)
  (:documentation "Destroys the DB associated with <object>."))

;; (defmethod destroy-db (db 
;;   (handler-case
;;       (clsql:disconnect)
;;     (SIMPLE-ERROR (se) (declare (ignore se)) nil))
;;   (handler-case
;;       (progn
;; 	(cond ((eq +db-type+ :mysql)
;; 	       (clsql:destroy-database +mysql-connection-string+ 
;; 				       :database-type +db-type+))
;; 	      (t nil ; TODO: erase file...
;; 		 (when (probe-file +db-name+) (delete-file +db-name+)))))
;;     (CLSQL-SYS:SQL-DATABASE-DATA-ERROR (se) (declare (ignore se)) nil)))

;;(make-instance 'simple-db)

;;;
;;; Things that change with mysql or sqlite3...
;;;

;; ;;(defparameter +db-type+ :mysql)
;; (defparameter +db-type+ :sqlite3)
;; (defparameter +db-primary-increment-flags+
;;   (if (eq +db-type+ :sqlite3)
;;       '(:not-null :unique :primary-key)
;;       '(:not-null :unique :primary-key :auto-increment)))
;; (defparameter +mysql-connection-string+
;;   `("localhost" ,+db-name+ "sjcarbon" ""))

;; ;; Different reader form now on.
;; ;;(clsql:enable-sql-reader-syntax)

;; ;;;
;; ;;; Database handling.
;; ;;;

;; ;;
;; (defun destroy-db ()
;;   ""
;;   (handler-case
;;       (clsql:disconnect)
;;     (SIMPLE-ERROR (se) (declare (ignore se)) nil))
;;   (handler-case
;;       (progn
;; 	(cond ((eq +db-type+ :mysql)
;; 	       (clsql:destroy-database +mysql-connection-string+ 
;; 				       :database-type +db-type+))
;; 	      (t nil ; TODO: erase file...
;; 		 (when (probe-file +db-name+) (delete-file +db-name+)))))
;;     (CLSQL-SYS:SQL-DATABASE-DATA-ERROR (se) (declare (ignore se)) nil)))

;; ;; Add tables.
;; (defun load-db-schema ()
;;   "Load the database with our files."

;;   ;;(if +debug+ (start-sql-recording :type :both))

;;   ;; Kanji table.
;;   (clsql:drop-table [kanji] :if-does-not-exist :ignore)
;;   (clsql:create-table [kanji]
;; 		      `(([id] integer ,@+db-primary-increment-flags+)
;; 			([level] integer :not-null)
;; 			([glyph] (varchar 1) :not-null)))

;;   ;; Words table.
;;   (clsql:drop-table [words] :if-does-not-exist :ignore)
;;   (clsql:create-table [words]
;; 		      `(([id] integer ,@+db-primary-increment-flags+)
;; 			([glyphs] (varchar 128) :not-null)
;; 			([reading] (varchar 128) :not-null)
;; 			([meaning] (varchar 1024) :not-null)))

;;   ;; Kanji-words table.
;;   (clsql:drop-table [kanji-words] :if-does-not-exist :ignore)
;;   (clsql:create-table [kanji-words]
;; 		      '(([kanji-id] integer :not-null)
;; 			([words-id] integer :not-null))))
