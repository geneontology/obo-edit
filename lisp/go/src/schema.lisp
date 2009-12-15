;;;;
;;;; GO database schema.
;;;;
;;;; (in-package :go-schema)
;;;; (enable-sql-reader-syntax)
;;;; (setf db (connect (list "localhost" "go_latest_lite" "" "") :database-type :mysql :if-exists :warn-old))
;;;; (start-sql-recording)
;;;; (setf got (get-term-by-acc "GO:0022008"))
;;;;
;;;; Will use to produce JS model via parenscript:
;;;; (moptilities:direct-slot-names (moptilities:get-class 'term))
;;;;

(require :clsql)
(require :clsql-mysql)
(require
(defpackage :go-schema
  (:use :cl
	:clsql
	:clsql-mysql
	:moptilities)
  (:export ;; instance_data
   :instance-data
   :release-name
   :release-type
   :release-notes

   ;; term
   :term
   :acc
   :name
   :term-type
   :is-root
   :is-root
   :is-relation
   :graph-child-relations
   :graph-parent-relations
   :term-associations
   ;;:parent-relations
   ;;:child-relations

   ;; term2term
   :term2term
   :relationship

   ;; graph_path
   :graph_path
   :subject
   :object
   :distance

   ;; association
   :association))
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
   (acc
    :accessor acc
    :db-constraints :not-null
    :initarg :acc
    :type (string 255))
   (name
    :accessor name
    :db-constraints :not-null
    :initarg :name
    :type (string 255))
   (term_type
    :accessor term-type
    :db-constraints :not-null
    :initarg :term-type
    :type (string 55))
   (is_obsolete
    :accessor is-obsolete
    :db-constraints :not-null
    :initarg :is-obsolete
    :initform 0
    :type integer)
   (is_root
    :accessor is-root
    :db-constraints :not-null
    :initarg :is-root
    :initform 0
    :type integer)
   (is_relation
    :accessor is-relation
    :db-constraints :not-null
    :initarg :is-relation
    :initform 0
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
			  :set t))
   (term-associations
    :accessor term-associations
    :db-kind :join
    :db-info (:join-class association
			  :home-key id
			  :foreign-key term_id
			  ;:retrieval :immediate
			  :set t)))
  (:base-table term))
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
			  :set t))
   (object
    :db-kind :join
    :db-info (:join-class term
			  :home-key term1_id
			  :foreign-key id
			  ;:retrieval :immediate
			  :set t))
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

;; association
(def-view-class association ()
  ((id
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (term_id
    :db-constraints :not-null
    :accessor term-id
    :initarg :term2-id
    :type integer)
   (gene_product_id
    :db-constraints :not-null
    :accessor gene-product-id
    :initarg :gene-product-id
    :type integer)
   (role_group
    :accessor role-group
    :initarg :role-group
    :type integer)
   (assocdate
    :accessor assocdate
    :initarg :assocdate
    :type integer)
   (source_db_id
    :accessor source-db-id
    :initarg :source-db-id
    :type integer)
   (term
    :accessor term
    :db-kind :join
    :db-info (:join-class term
			  :home-key term_id
			  :foreign-key id
			  ;:retrieval :immediate
			  :set t))
   (gene-product
    :accessor gene-product
    :db-kind :join
    :db-info (:join-class gene-product
			  :home-key gene_product_id
			  :foreign-key id
			  ;:retrieval :immediate
			  :set t)))
  (:base-table association))

;; association
(def-view-class gene-product ()
  ((id
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (symbol
    :accessor term-type
    :db-constraints :not-null
    :initarg :term-type
    :type (string 128))
   (dbxref_id
    :db-constraints :not-null
    :accessor dbxref-id
    :initarg :dbxref-id
    :type integer)
   (species_id
    :accessor species-id
    :initarg :species-id
    :type integer)
   (type_id
    :accessor type-id
    :initarg :type-id
    :type integer)
   (full_name
    :accessor full-name
    :initarg :full-name
    :type string)
   (gene-product-associations
    :accessor gene-product-associations
    :db-kind :join
    :db-info (:join-class association
			  :home-key id
			  :foreign-key gene_product_id
			  ;:retrieval :immediate
			  :set t)))
  (:base-table gene_product))

;; evidence
(def-view-class evidence ()
  ((id
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (code
    :accessor code
    :db-constraints :not-null
    :initarg :code
    :type (string 8))
   (association_id
    :db-constraints :not-null
    :accessor association-id
    :initarg :association-id
    :type integer)
   (dbxref_id
    :db-constraints :not-null
    :accessor dbxref-id
    :initarg :dbxref-id
    :type integer)
   (seq_acc
    :accessor seq-acc
    :initarg :seq-acc
    :type (string 255)))
  ;; TODO: JOINs
  (:base-table evidence))

;; dbxref
(def-view-class dbxref ()
  ((id
    :db-kind :key
    :db-constraints :primary-key
    :initform nil
    :type integer)
   (xref_dbname
    :accessor xref-dbname
    :db-constraints :not-null
    :initarg :xref-dbname
    :type (string 55))
   (xref_key
    :accessor xref-key
    :db-constraints :not-null
    :initarg :xref-key
    :type (string 255))
   (xref_keytype
    :accessor xref-keytype
    :initarg :xref-keytype
    :type (string 32))
   (xref_desc
    :accessor xref-desc
    :initarg :xref-desc
    :type (string 255)))
  ;; TODO: JOINs
  (:base-table dbxref))

;;;
;;; Local toolz.
;;;

;; (defun get-term-by-acc (acc-string)
;;   (caar (select 'term :where [= [term.acc] acc-string] :caching nil)))
