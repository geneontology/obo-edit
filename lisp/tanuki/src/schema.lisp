;;;; -*- mode: Lisp -*-
;;;;
;;;; Database schema.
;;;;
;;;; TODO: Add reference (for referer) linked to arguments (yes, think
;;;; about it...)
;;;;

(defpackage :tanuki-schema
  (:use :cl
	:postmodern)
  (:export
   ;; NS Globals
   :*tables*
   :*sequences*
   ;; Tables.
   :meta
   :page
   :argument
   :argument-set
   :hit
   :comment
   :message
   :reference
   ;; Slots.
   :start
   :target
   :data
   :id
   :url
   :internal
   :page-id
   :raw-url
   :clean-url
   :reference
   :request-method
   :request-type
   :mark
   :todo
   :argument-set-id
   :name
   :value
   :filesystem
   :wait
   :date
   :agent
   :code
   :flagged
   :success
   :hit-id
   :comment-type
   :text
   :source   
   ;; Joins
   ;; ...
   ))
(in-package :tanuki-schema)

;;;
;;; Schema.
;;;

(defvar *tables* '(meta page argument argument-set hit comment message reference)
  "All the tables that are used in Tanuki's database.")
(defvar *sequences* '(page-id-seq argument-id-seq argument-set-id-seq hit-id-seq comment-id-seq message-id-seq reference-id-seq)
  "All the sequences that are used in Tanuki's database.")

;; TODO/BUG/DEBUG: You don't know this, you don't see this.
(defun %reset-lists ()
  "You don't know this, you don't see this." 
  (setf *tables* '(meta page argument argument-set hit comment message reference))
  (setf *sequences* '(page-id-seq argument-id-seq argument-set-id-seq hit-id-seq comment-id-seq message-id-seq reference-id-seq))
  t)

;; (dao-table-definition 'meta) looks correct...
(defclass meta ()
  ((start
    :accessor start
    :col-type bigint
    :initarg :start)
   (target
    :accessor target
    :col-type string
    :initarg :target)
   (data
    :accessor data
    :col-type string
    :initarg :data))
  (:metaclass dao-class))

;; (dao-table-definition 'page) looks correct...
(defclass page ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (url ; TODO: this has gots to be unique, does this do it?
    :accessor url
    :col-type string
    :initarg :url
    :unique t)
   (internal
    :accessor internal
    :col-type bigint
    :initarg :internal))
  (:metaclass dao-class)
  (:keys id url))

;; (dao-table-definition 'argument-set) looks correct...
(defclass argument-set ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (page-id
    :accessor page-id
    :col-type bigint
    :initarg :page-id)
   (raw-url
    :accessor raw-url
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :raw-url
    :documentation "The URL from first contact, if any.")
   (clean-url
    :accessor clean-url
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :clean-url
    :documentation "The raw URL cleaned and with an ordered query, if any.")
   (reference
    :accessor reference
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :reference
    :documentation "URL where this argument set was discovered.")
   (request-method
    :accessor request-method
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :request-method
    :documentation "Request method--should be something like POST or GET...")
   (request-type
    :accessor request-type
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :request-type
    :documentation "Type of access (e.g. link or form).")
   (mark
    :accessor mark
    :col-type bigint
    :col-default 0
    :documentation "A scratch mark; for mandating, remembering, etc.")
   (todo
    :accessor todo
    :col-type bigint
    :col-default 0
    :initform 0
    :initarg :todo
    :documentation "Is it on our todo list?"))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'argument) looks correct...
(defclass argument ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (argument-set-id
    :accessor argument-set-id
    :col-type bigint
    :initarg :argument-set-id)
   ;; (reference-id
   ;;  :accessor argument-set-id
   ;;  :col-type bigint
   ;;  :initarg :argument-set-id)
   (name
    :accessor name
    :col-type string
    :initarg :name
    :initform (error "argument requires a :name")
    :documentation "...")
   (value
    :accessor value
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :value
    :documentation "...")
   (filesystem
    :accessor filesystem
    :col-type bigint
    :col-default 0
    :initform 0
    :initarg :filesystem
    :documentation "Whether or not it is an internal value, or on the
    filesystem for upload/POST. Default to internal value."))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'hit) looks correct...
(defclass hit ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (argument-set-id
    :accessor argument-set-id
    :col-type bigint
    :initform (error "need an arg  set id here...")
    :initarg :argument-set-id)   
   (wait
    :accessor wait
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :wait
    :documentation "Hopefully the time to completion for a request in seconds.")
   (date
    :accessor date
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :date
    :documentation "The approximate data/time of the hit in.")
   (agent
    :accessor agent
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :agent
    :documentation "Token id for an invididual agent.")
   (code
    :accessor code
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :code)
   (flagged
    :accessor flagged
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :flagged)
   (success
    :accessor success
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :success))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'comment) looks correct...
(defclass comment ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (hit-id
    :accessor hit-id
    :col-type bigint
    :initarg :hit-id)
   (comment-type
    :accessor comment-type
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :comment-type
    :documentation "...")
   (text
    :accessor text
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :text
    :documentation "..."))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'message) looks correct...
(defclass message ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (source
    :accessor from
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :source
    :documentation "The source of the message")
   (date
    :accessor date
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :date
    :documentation "The approximate data/time of the hit in.")
   (text
    :accessor text
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :text
    :documentation "The text of the log/message."))
  (:metaclass dao-class)
  (:keys id))

;; NOTE: Currently unused in favor of just embedding it in
;; argument-set.
;; (dao-table-definition 'reference) looks correct...
(defclass reference ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (url
    :accessor url
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :url
    :documentation "..."))
  (:metaclass dao-class)
  (:keys id))
