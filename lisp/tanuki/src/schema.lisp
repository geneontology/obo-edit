;;;; -*- mode: Lisp -*-
;;;;
;;;; Database schema.
;;;;
;;;; TODO: Add reference (for referer) linked to arguments (yes, think
;;;; about it...)
;;;;
;;;; TODO: finish set-connection
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
   :arguments
   :hit
   ;;:reference
   ;; Slots.
   :start
   :target
   :id
   :url
   :internal
   :mandated
   :tried
   :page-id
   :arguments-id
   :referer
   :wait
   :date
   :agent
   :code
   :flagged
   :success
   ))
(in-package :tanuki-schema)

;;;
;;; Schema.
;;;

(defvar *tables* '(meta page arguments hit)
  "All the tables that are used in Tanuki's database.")
(defvar *sequences* '(page-id-seq arguments-id-seq hit-id-seq)
  "All the sequences that are used in Tanuki's database.")

;; (dao-table-definition 'meta) looks correct...
(defclass meta ()
  ((start
    :accessor start
    :col-type bigint
    :initarg :start)
   (target
    :accessor target
    :col-type string
    :initarg :target))
  (:metaclass dao-class))
   
(defclass page ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (url
    :accessor url
    :col-type string
    :initarg :url)
   (internal
    :accessor internal
    :col-type bigint
    :initarg :internal)
   (mandated
    :accessor mandated
    :col-type bigint
    :col-default 0)
   (tried
    :accessor tried
    :col-type bigint
    :col-default 0))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'arguments) looks correct...
(defclass arguments ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (page-id
    :accessor page-id
    :col-type bigint
    :initarg :page-id)
   (arguments ;; TODO: this will be handy...
    :accessor arguments
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :arguments))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'hit) looks correct...
(defclass hit ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (page-id
    :accessor page-id
    :col-type bigint
    :initarg :page-id)
   (arguments-id
    :accessor arguments-id
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :arguments-id)   
   (referer
    :accessor referer
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :referer)
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
