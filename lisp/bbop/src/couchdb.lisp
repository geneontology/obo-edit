;;;;
;;;; A simple, and livable, CouchDB interface.
;;;;
;;;; TODO: convert to CLOS...
;;;;

;;(require :bbop-utils)
(require :cl-ppcre)
(require :cl-json)
(require :cl-fad)
;;(require :parenscript)
(require :drakma)

(defpackage :bbop-couchdb
  (:use :cl
	:json
	:drakma
	:bbop-utils
	:bbop-json)
  (:export :connect-to
	   :create-db
	   :add-doc
	   :delete-doc
	   :get-uuid))
(in-package :bbop-couchdb)


;;;
(defvar *cdb* "http://localhost:5984"
  "CouchDB location.")

;; Resty, so don't really have to worry about a persistant connection :)
(defun connect-to (new-cdb-url)
  (setf *cdb* new-cdb-url))

(defun json-to-hash (json-str)
  ""
  (let ((hash (make-hash-table)))
    (mapcar #'(lambda (x)
		(setf (gethash (car x) hash) (cdr x)))
	    (json:decode-json-from-string json-str))
    hash))

(defun create-db (db-name)
  ;;(multiple-value-bind (json code struct puri five six seven)
  ;;(multiple-value-bind (json code)
  (multiple-value-bind (json)
      (http-request (string-merge *cdb* "/" db-name) :method :put)
    ;;(json:decode-json-from-string json)))
    (let* ((hash (json-to-hash json))
	   (db-error (gethash :error hash))
	   (db-ok (gethash :ok hash)))
      (if db-error
	  (values nil (gethash :reason hash))
	  (values t db-ok)))))

(defun add-doc (db-name object &optional (id nil))
  (multiple-value-bind (json)
      (if (null id)
	  (http-request (string-merge *cdb* "/" db-name "/")
			:method :post
			:content (json:encode-json-to-string object))
	  (http-request (string-merge *cdb* "/" db-name "/" id)
			:method :put
			:content (json:encode-json-to-string object)))
    (let* ((hash (json-to-hash json))
	   (db-error (gethash :error hash))
	   ;;(db-ok (gethash :ok hash)))
	   )
      (if db-error
	  (values nil (gethash :reason hash))
	  (values t (gethash :rev hash))))))

;; BUG: needs rev...
(defun delete-doc (db-name id)
  (multiple-value-bind (json)
      (http-request (string-merge *cdb* "/" db-name "/" id)
		    :method :delete)
    (let* ((hash (json-to-hash json))
	   (db-error (gethash :error hash))
	   ;;(db-ok (gethash :ok hash)))
	   )
      (if db-error
	  (values nil (gethash :reason hash))
	  (values t (gethash :ok hash))))))


;; BUG?: is this only available from 0.9+?
(defun get-uuid (&optional (num 1))
  (with-input-from-string (s (http-request (string-merge *cdb* "/_uuids?" (write-to-string num)) :method :get))
    (cadar (decode-json s))))
;;  (string-merge *cdb* "/_uuids?" (write-to-string num)))
