;;;;
;;;; NOTE: we're screwing with the reader a lot in here.
;;;; NOTE: we're using SQLite-isms to make the boolean stuff work.
;;;;
;;;; TODO: Separate tanuki-db into db and tanuki-store.
;;;;

(require :clsql)
(require :clsql-sqlite3)
(defpackage :tanuki-db
  (:use :cl
	:tanuki-utils
	:tanuki-file
	:clsql-sqlite3
	:clsql)
  (:export :meta
	   :page
;;	   :form
;;	   :form-argument

	   :tanuki-database
	   :initialize-tanuki-database
	   :open-tanuki-database
	   :create-tanuki-database
	   :reconnect-tanuki-database
	   :close-tanuki-database

	   ;:enter-meta
	   :get-database
	   :get-meta
	   :get-name
	   :get-target
	   :get-version

	   :enter-page
	   :update-page
	   :mark-page-as-failed
	   :mark-page-as-odd
	   :mark-page-as-visited
	   :mark-page-as-not-mandated
	   :mark-page-with-date
	   :mark-page-with-time

	   :sql-engine
	   :url-in-db-p
	   :get-page-from-url
	   :get-page-count
	   :get-unvisited-page-count
	   :get-internal-page-count
	   :get-internal-unvisited-page-count
	   :get-bad-page-count
	   :get-odd-page-count
	   :get-nth-page
	   :get-nth-unvisited-page
	   :get-nth-internal-page
	   :get-nth-internal-unvisited-page
	   :get-all-pages
	   :get-all-internal-pages
	   :get-visited-pages
	   :get-unvisited-pages
	   :get-internal-unvisited-pages
	   :get-url
	   :get-referer))
(in-package :tanuki-db)

;; Redoing some nasty stuff in oder to get auto-increment work like we
;; want under sqlite3. WARNING: This may well break portability.
(defmethod clsql-sys:database-get-type-specifier ((type (eql 'integer)) 
                                                  args database 
                                                  (db-type (eql :sqlite3)))
  (declare (ignore database db-type))
  (if args
      (format nil "INTEGER(~A)" (car args))
    "INTEGER"))
(defmethod clsql-sys::database-pkey-constraint ((class 
						 clsql-sys::standard-db-class) 
                                                (database 
						 clsql-sqlite3::database)))

;;;
;;; Schema.
;;;

;; Class for containing the the meta info for the DB.
(def-view-class meta ()
  ((version ; the starting URL
;;    :db-constraints :not-null
    :initform 1
    :type integer)
   (target ; the starting URL
;;    :db-constraints :not-null
    :initarg :target
    :type (string 1024))
   (name ; the proper name for the webapp
;;    :db-constraints :not-null
    :initarg :name
    :type (string 1024))))

(def-view-class page ()
  ((id ; unique ID (url as key would interfere with paths)
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (url ; the string URL.
;;    :db-constraints :not-null
    :initarg :url
    :type (string 1024))
   (referer ; ID of the refering page
;;    :db-constraints :not-null
    :initarg :referer
    :type (string 1024))
   (visited ; whether or not we've been here before.
;;    :db-constraints :not-null
    :initarg :visited
    :type boolean)
   ;; Whether or not taking the URL caused an error. May be null if
   ;; not yet visited.
   (failed
;;    :db-constraints :not-null
    :initarg :failed
    :type boolean)
   ;; Whether or not there was somthing...odd...about the page.
   (odd
;;    :db-constraints :not-null
    :initarg :odd
    :type boolean)
   ;; Whether or not this URL must be tested (probably read in from a
   ;; file for) compatability testing. A way of forcing attention.
   (mandated
;;    :db-constraints :not-null
    :initarg :mandated
    :type boolean)
   ;; Whether of not the URL is internal to the application. Should be
   ;; relative to the info in META.
   (internal
;;    :db-constraints :not-null
    :initarg :internal
    :type boolean)
   (comment ; surprise me!
    ;:db-constraints :not-null
    :initarg :comment
    :type (string 1024))
   (form
    ;;:db-constraints :not-null
    :initarg :form
    :type boolean)
   (time
    ;;:db-constraints :not-null
    :initarg :time
    :type integer)
   (date
;;    :db-constraints :not-null
    :initarg :date
    :type integer)))

;; TODO/BUG: visits should be separated out and able 
;; to be run against forms as well
;; (def-view-class visit ()
;;   ((id
;;     :db-kind :key
;;     :db-constraints :primary-key
;;     :initform nil
;;     :type integer)
;;    ;; Whether or not taking the URL caused an error. May be null if
;;    ;; not yet visited.
;;    (failed
;;     :db-constraints :not-null
;;     :initarg :failed
;;     :type boolean)
;;    ;; Whether or not there was somthing...odd...about the page.
;;    (odd
;;     :db-constraints :not-null
;;     :initarg :odd
;;     :type boolean)
;;    (time
;;     :db-constraints :not-null
;;     :initarg :time
;;     :type integer)
;;    (date
;;     :db-constraints :not-null
;;     :initarg :date
;;     :type integer)))

;; (def-view-class form ()
;;   ((id
;;     :db-kind :key
;;     :db-constraints :primary-key
;;     :initform nil
;;     :type integer)
;;   (page-id
;;     :db-constraints :not-null
;;     :initarg :page-id
;;     :type integer)
;;   (target
;;     :db-constraints :not-null
;;     :initarg :target
;;     :type (string 1024))
;;   (type
;;     :db-constraints :not-null
;;     :initarg :type
;;     :type (string 1024))))

;; (def-view-class form-argument ()
;;   ((id
;;     :db-kind :key
;;     :db-constraints :primary-key
;;     :initform nil
;;     :type integer)
;;   (form-id
;;     :db-constraints :not-null
;;     :initarg :form-id
;;     :type integer)
;;   (file
;;     :db-constraints :not-null
;;     :initarg :file
;;     :type (string 1024))
;;   (name
;;     :db-constraints :not-null
;;     :initarg :name
;;     :type (string 1024))
;;   (value
;;     :db-constraints :not-null
;;     :initarg :value
;;     :type (string 1024))))

;;;
;;; A trivial class so we can juggle multiple databases. Most of the
;;; interesting bits will be in the database anyways.
;;;

(defclass tanuki-database ()
  ((database
    :initform nil
    :accessor database
    :documentation "All the info will be in the DB.")))
;;   (name
;;    :initarg :name
;;    :initform (error "need a name, buddy")
;;   ;;:initform nil
;;    :accessor name
;;    :documentation "Human name of the database.")))

;;;
;;; DB Connections.
;;;

;;(defgeneric initialize-database (database db-string)
;;  (:documentation "Initializes a DB for our usage."))
(defmethod create-tanuki-database ((dbo tanuki-database)
				   &key (target nil)
				   (dbfile nil)
				   (name nil))
  "Initializes a database, tables and all."
  (key-check "A required key arg was not specified." target dbfile name)
  (let ((db (connect (list dbfile) :database-type :sqlite3
				   :if-exists :warn-old)))
    ;; If the tables exist, nuke 'em.
    (drop-table 'meta :database db :if-does-not-exist :ignore)
    (drop-table 'page :database db :if-does-not-exist :ignore)
;;    (drop-table 'form :database db :if-does-not-exist :ignore)
;;    (drop-table 'form-argument :database db :if-does-not-exist :ignore)
    ;; Add the class views.
    (create-view-from-class 'meta :database db)
    (create-view-from-class 'page :database db)
;;    (create-view-from-class 'form :database db)
;;    (create-view-from-class 'form-argument :database db)
    ;; Toss it out to the slot.
    (with-slots (database name) dbo (setf database db)))
  ;; Add the meta and seed.
  (enter-meta dbo :target target :name name :version 1)
  (enter-page dbo :url target :referer "" :internal t))

(defmethod reconnect-tanuki-database ((dbo tanuki-database)
				      &key (dbfile nil))
  "Open an already initialized database into a dbo."
  ;; TODO: Add error checking to make sure that this is indeed initialized.
  (key-check "Requires a :dbfile key arg." dbfile)
  (with-slots (database)
	      dbo
	      (setf database (connect (list dbfile) :database-type :sqlite3
						    :if-exists :warn-old))))

(defmethod close-tanuki-database ((dbo tanuki-database))
  "Close the connection to the database."
  ;; TODO: Add error checking to make sure that this is indeed initialized.
  (if dbo
      (with-slots (database)
		  dbo
		  (if database
		      (disconnect :database database)
		    nil))))

;;;
;;; Write and read meta info.
;;;

(defmethod enter-meta ((dbo tanuki-database)
		       &key (target nil)
		       (name nil)
		       (version nil))
  "Insert/update the meta info."
  (key-check "You must specify all key args." target name version)
  (let ((meta (get-meta dbo)))
    (cond
     (meta ; We already have meta info.
      (progn
	(setf (slot-value meta 'target) target)
	(setf (slot-value meta 'name) name)
	(setf (slot-value meta 'version) version)
	(update-records-from-instance meta :database (get-database dbo))))
     (t ; Insert new meta info.
      (update-records-from-instance (make-instance 'meta :target target
							 :name name
							 :version version)
				    :database (get-database dbo))))))

(defmethod get-meta ((dbo tanuki-database))
  "Returns the meta object."
  (car (select 'meta :database (get-database dbo)
		     :caching nil :flatp t
		     :limit 1)))

(defmethod get-database ((dbo tanuki-database))
  "Return the database object."
  (with-slots (database) dbo database))

(defmethod get-name ((dbo tanuki-database))
  "Return the name string."
  (slot-value (get-meta dbo) 'name))

(defmethod get-version ((dbo tanuki-database))
  "Return the version number."
  (slot-value (get-meta dbo) 'version))

(defmethod get-target ((dbo tanuki-database))
  "Return the target string."
  (slot-value (get-meta dbo) 'target))

;;;
;;; Page entry, update, and reading.
;;;

(defmethod enter-page ((dbo tanuki-database)
		       &key (url nil) (referer nil) (visited nil)
		       (failed nil) (odd nil) (mandated nil) (internal nil)
		       (date 0))
  "Insert a page."
  (key-check "You must specify the :url key arg." url)
  (let ((in-page (make-instance 'page
				:visited visited
				:failed failed
				:mandated mandated
				:odd odd
				:internal internal
				:date date
				:referer referer
				:url url)))
    (update-records-from-instance in-page :database (get-database dbo))))

;; TODO: There mush be a better way of doing this...some macro to
;; juggle the keys...
(defmethod update-page ((dbo tanuki-database)
			&key (page nil)
			(visited nil visited-supplied-p)
			(failed nil failed-supplied-p)
			(internal nil internal-supplied-p)
			(url nil url-supplied-p)
			(odd nil odd-supplied-p)
			(mandated nil mandated-supplied-p)
			(referer nil referer-supplied-p)
			(date nil date-supplied-p)
			(time nil time-supplied-p))
  "Update a page. "
  (key-check "You must specify the :page key arg." page)
  (when visited-supplied-p
    (setf (slot-value page 'visited) (if visited t nil)))
  (when failed-supplied-p
    (setf (slot-value page 'failed) (if failed t nil)))
  (when odd-supplied-p
    (setf (slot-value page 'odd) (if odd t nil)))
  (when internal-supplied-p
    (setf (slot-value page 'internal) (if internal t nil)))
  (when mandated-supplied-p
    (setf (slot-value page 'mandated) (if mandated t nil)))
  (when referer-supplied-p
    (setf (slot-value page 'referer) (if referer t nil)))
  (when date-supplied-p
    (setf (slot-value page 'date) (if date date 0))) ; TODO/BUG?
  (when time-supplied-p
    (setf (slot-value page 'time) (if time time 0))) ; TODO/BUG?
  (when (and url-supplied-p url)
    (setf (slot-value page 'url) url))
  (update-records-from-instance page :database (get-database dbo))
  nil)

(defmethod mark-page-as-visited ((dbo tanuki-database)
				 (page page))
  "Mark a specified page in the specified database as visited."
  (update-page dbo :page page :visited t))

(defmethod mark-page-as-failed ((dbo tanuki-database)
				(page page))
  "Mark a specified page in the specified database as failed."
  (update-page dbo :page page :failed t))

(defmethod mark-page-as-odd ((dbo tanuki-database)
			     (page page))
  "Mark a specified page in the specified database as odd."
  (update-page dbo :page page :odd t))

(defmethod mark-page-as-not-mandated ((dbo tanuki-database)
				      (page page))
  "Mark a specified page in the specified database as not mandated."
  (update-page dbo :page page :mandated nil))

(defmethod mark-page-with-date ((dbo tanuki-database)
				(page page))
  "Mark a specified page with the date."
  (update-page dbo :page page :date (get-universal-time)))

(defmethod mark-page-with-time ((dbo tanuki-database)
				(page page) time)
  "Mark a specified page with the specified time."
  (update-page dbo :page page :time time))

;;;
;;; SQL engine.
;;; TODO: This can certainly be written more compactly as well...
;;;

;; TODO: Ooo! Ooo! This can all be done with plists...
(defun make-where-clause (pairs)
  "Turn a list of table name and bool pairs into appropriate CLSQL and
expression."
  (sql-operation
   'and (loop for p in pairs
	      collect
	      (sql-operation
	       '= (sql-expression :attribute (car p))
	       (let ((val (cadr p)))
		 (cond 
		  ((stringp val) val)
		  (val "t")
		  (t "f")))))))

;; ;; WARNING/BUG: the "-1"s are SQLite-isms. They should be jimmied out
;; ;; for other backends. Also (see above), this can possibly be done
;; ;; easier with plists.
;;      ;; getf 
;;      ;; remf
;; And a select writing macro...
;;      (select 'page :database database
;; 		   :caching nil
;; 		   :flatp t
;; 		   :limit limit
;; 		   :offset offset
;; 		   :where ,where-clause)     
(defmethod sql-engine (dbo &key (count nil) (limit -1) (offset -1)
			   (mandated nil mandated-supplied-p)
			   (failed nil failed-supplied-p)
			   (odd nil odd-supplied-p)
			   (visited nil visited-supplied-p)
			   (internal nil internal-supplied-p)
			   (url "" url-supplied-p))
  "A simplfied way to deal with simple data queries on page."
  ;; Generate the list of arguments.
  ;; TODO: generate where clause off of table-plist.
  (let ((query-list '()))
    (if failed-supplied-p
	(push (list 'failed (if failed t nil)) query-list))
    (if odd-supplied-p
	(push (list 'odd (if odd t nil)) query-list))
    (if mandated-supplied-p
	(push (list 'mandated (if mandated t nil)) query-list))
    (if visited-supplied-p
	(push (list 'visited (if visited t nil)) query-list))
    (if internal-supplied-p
	(push (list 'internal (if internal t nil)) query-list))
    (if url-supplied-p
	(push (list 'url url) query-list))
    ;; Make the appropriate :where clause from the list.
    (let ((where-clause (if query-list (make-where-clause query-list) nil)))
      ;; Select what kind of clause we're making and return the
      ;; generated code for consumption.
      ;; TODO: these can be compacted more by fixing make-where-clause
      #.(locally-enable-sql-reader-syntax)
      (with-slots
       (database) dbo
       (cond
	((and count where-clause)
	 (car (select [count [*]] :database database
				  :caching nil
				  :flatp t
				  :from [page]
				  :where where-clause)))
	(count
	 (car (select [count [*]] :database database
				  :caching nil
				  :flatp t
				  :from [page])))
	(where-clause
	 (select 'page :database database
		       :caching nil
		       :flatp t
		       :limit limit
		       :offset offset
		       :where where-clause))
	(t
	 (select 'page :database database
		       :caching nil
		       :flatp t
		       :limit limit
		       :offset offset))))
      #.(restore-sql-reader-syntax-state))))

;;;
;;; Database convenience methods.
;;;

(defmethod url-in-db-p ((dbo tanuki-database) ustr)
  "Returns true if the URL is in the db, nil otherwise."
  (let ((count (sql-engine dbo :count t :url ustr)))
    (if (= count 0) nil t)))

(defmethod get-page-from-url ((dbo tanuki-database) ustr)
  "Returns true if the URL is in the db, nil otherwise."
  (let ((result (sql-engine dbo :url ustr :visited nil)))
    (if result (car result) nil)))

(defmethod get-page-count ((dbo tanuki-database))
  "Return the number of pages."
  (sql-engine dbo :count t))

(defmethod get-unvisited-page-count ((dbo tanuki-database))
  "Return the number of unvisited pages."
  (sql-engine dbo :visited nil :count t))

(defmethod get-internal-page-count ((dbo tanuki-database))
  "Return the number of unvisited pages."
  (sql-engine dbo :internal t :count t))

(defmethod get-internal-unvisited-page-count ((dbo tanuki-database))
  "Return the number of unvisited pages."
  (sql-engine dbo :internal t :visited nil :count t))

(defmethod get-bad-page-count ((dbo tanuki-database))
  "Return the number of visited pages that failed."
  (sql-engine dbo :failed t :visited t :count t))

(defmethod get-odd-page-count ((dbo tanuki-database))
  "Return the number of visited pages that are odd."
  (sql-engine dbo :odd t :visited t :count t))

(defmethod get-nth-page ((dbo tanuki-database) n)
  "Return the nth PAGE in the database, or nil. Starts from 1."
  (let ((page-list (sql-engine dbo :limit 1 :offset (1- n))))
    (if page-list (car page-list)
      nil)))

(defmethod get-nth-unvisited-page ((dbo tanuki-database) n)
  "Return the nth unvisited PAGE in the database, or nil. Starts from 1."
  (let ((page-list (sql-engine dbo :limit 1 :offset (1- n) :visited nil)))
    (if page-list (car page-list)
      nil)))

(defmethod get-nth-internal-page ((dbo tanuki-database) n)
  "Return the nth internal PAGE in the database, or nil. Starts from 1."
  (let ((page-list (sql-engine dbo :limit 1 :offset (1- n) :internal t)))
    (if page-list (car page-list)
      nil)))

(defmethod get-nth-internal-unvisited-page ((dbo tanuki-database) n)
  "Return the nth internal unvisited PAGE in the database, or nil. Starts from 1."
  (let ((page-list (sql-engine dbo :limit 1 :offset (1- n) :visited nil :internal t)))
    (if page-list (car page-list)
      nil)))

(defmethod get-all-pages ((dbo tanuki-database))
  "Return a list of PAGES in the database (that meet a specified criteria)."
  (sql-engine dbo))

(defmethod get-all-internal-pages ((dbo tanuki-database))
  "Return a list of PAGES in the database (that meet a specified criteria)."
  (sql-engine dbo :internal t))

(defmethod get-visited-pages ((dbo tanuki-database))
  "Return a list of visited page URLs in the database."
  (sql-engine dbo :visited t))

;; NOTE: we're using SQLite-isms to make the boolean stuff work.
(defmethod get-unvisited-pages ((dbo tanuki-database))
  "Return a list of unvisited page URLs in the database."
  (sql-engine dbo :visited nil))

(defmethod get-internal-unvisited-pages ((dbo tanuki-database))
  "Return a list of internal unvisited page URLs in the database."
  (sql-engine dbo :internal t :visited nil))

;;;
;;; Page convenience methods.
;;;

(defmethod get-url ((page page))
  "Return the name of a page."
  (with-slots
   (url) page url))

(defmethod get-referer ((page page))
  "Return the name of a page."
  (with-slots
   (referer) page referer))

;;;
;;; Odds and ends...
;;;



;;
(defun print-page (page)
  "Pretty print a PAGE (probably for debugging)."
  (print (describe page)))
