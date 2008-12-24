;;;;
;;;; GO database schema.
;;;;
;;;; (start-sql-recording)
;;;; (enable-sql-reader-syntax)
;;;;

(clc:clc-require :clsql)
(clc:clc-require :clsql-mysql)
(defpackage :go-schema
  (:use :cl
	:clsql-mysql
	:clsql)
  (:export :instance-data
	   :term
	   :term2term
	   :graph_path
	   :acc
	   :name
	   :term-type
	   :subject
	   :object
	   :parent-relations
	   :child-relations
	   :graph-child-relations
	   :graph-parent-relations
	   :relationship
	   :distance))
(in-package :go-schema)


;; Class for containing the the meta info for the DB.
(def-view-class instance-data ()
  ((release_name
    :accessor release-name
    :initform nil
    :type (string 255))
   (release_type
    :accessor release-type
    :initform nil
   :type (string 255))
   (release_notes
    :accessor release-notes
    :initform nil
    :type string))
  (:base-table instance_data))

;; GO term.
(def-view-class term ()
  ((id
    :accessor id
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (name
    :accessor name
    :db-constraints :not-null
    :initarg :name
    :type (string 255))
   (term_type
    :accessor term-type
    :db-constraints :not-null
    :initarg :term_type
    :type (string 55))
   (acc
    :accessor acc
    :db-constraints :not-null
    :initarg :acc
    :type (string 255))
   (is_obsolete
    :accessor is-obsolete
    :db-constraints :not-null
    :initarg :is-obsolete
    :type integer)
   (is_root
    :accessor is-root
    :db-constraints :not-null
    :initarg :is-root
    :type integer)
   (graph-child-relations
    :accessor graph-child-relations
    :db-kind :join
    :db-info (:join-class graph-path
	      :home-key id
	      :foreign-key term1_id
	      ;:retrieval :immediate
	      :set t))
   (graph-parent-relations
    :accessor graph-parent-relations
    :db-kind :join
    :db-info (:join-class graph-path
	      :home-key id
	      :foreign-key term2_id
	      ;:retrieval :immediate
	      :set t)))
;;    (child-relations
;;     :accessor child-relations
;;     :db-kind :join
;;     :db-info (:join-class term2term
;; 	      :home-key id
;; 	      :foreign-key term1_id
;; 	      ;:retrieval :immediate
;; 	      :set t))
;;    (parent-relations
;;     :accessor parent-relations
;;     :db-kind :join
;;     :db-info (:join-class term2term
;; 	      :home-key id
;; 	      :foreign-key term2_id
;; 	      ;:retrieval :immediate
;; 	      :set t)))
  (:base-table term))

;; term2term table for the relationship between terms
(def-view-class term2term ()
  ((id
    :accessor id
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (relationship_type_id
    :db-constraints :not-null
    :accessor relationship-type-id
    :initarg  relationship-type-id
    :type integer)
   (relationship
    :accessor relationship
    :db-kind :join
    :db-info (:join-class term
	      :home-key relationship_type_id
	      :foreign-key id
	      ;:retrieval :immediate
	      :set t))
   (term1_id
    :accessor term1-id
    :db-constraints :not-null
    :initarg :term1-id
    :type integer)
   (term2_id
    :accessor term2-id
    :db-constraints :not-null
    :initarg :term2-id
    :type integer)
   (complete
    :accessor complete
    :db-constraints :not-null
    :initarg :complete
    :type integer)
   (subject
    :accessor subject
    :db-kind :join
    :db-info (:join-class term
	      :home-key term2_id
	      :foreign-key id
	      ;:retrieval :immediate
	      :set t))
   (object
    :accessor object
    :db-kind :join
    :db-info (:join-class term
	      :home-key term1_id
	      :foreign-key id
	      ;:retrieval :immediate
	      :set t)))
  (:base-table term2term))

;; graph_path table to get the graph neighborhood around a term
(def-view-class graph-path ()
  ((id
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (subject
    :db-kind :join
    :db-info (:join-class term
	      :home-key term2_id
	      :foreign-key id
	      ;:retrieval :immediate
	      :set nil))
   (object
    :db-kind :join
    :db-info (:join-class term
	      :home-key term1_id
	      :foreign-key id
	      ;:retrieval :immediate
	      :set nil))
   (term1_id
    :db-constraints :not-null
    :accessor term1-id
    :initarg :term1-id
    :type integer)
   (term2_id
    :db-constraints :not-null
    :accessor term2-id
    :initarg :term2-id
    :type integer)
   (distance
    :db-constraints :not-null
    :initarg :distance
    :type integer))
  (:base-table graph_path))
