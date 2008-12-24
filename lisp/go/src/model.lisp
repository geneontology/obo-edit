;;;;
;;;; Abstraction model for GO Objects.
;;;;
;;;; TODO: perhaps we should have a relationship object instead of
;;;; just the naked string--might make things easier when we have
;;;; graph_path and t2t fixed-up a bit...
;;;;

;;(clc:clc-require :go-engine)
(clc:clc-require :clsql)
(clc:clc-require :clsql-mysql)
(clc:clc-require :go-schema)
(clc:clc-require :go-engine)
(defpackage :go-model
  (:use :cl
	:clsql-mysql
	:clsql
	:go-schema
	:go-engine)
  (:export :go-object
	   :go-term
	   :info
	   :print-info
	   :create-term))
(in-package :go-model)

;;;
;;; ...
;;;

;; The base that other GO objects will be build on.
(defclass go-object ()
  ((id
    :documentation "The unique ID of the socket object(s)."
    :initform nil
    :accessor id
    :initarg :id)
   (engine
    :documentation "A connected GO engine."
    :initform nil
    :accessor engine
    :initarg :engine)
      ))
;;    (type
;;     :documentation ""
;;     :initform nil
;;     :accessor type
;;     :initarg :type)))

;; A simple GO term.  
(defclass go-term (go-object)
  ((term
    :documentation "cl-sql term object"
    ;;:initform nil 
    :accessor term
    :initarg :term)
   ))
;;    (type
;;     :initform "term")))

(defclass go-relation (go-object)
  ((subject
    :documentation "subject go-term"
    ;;:initform nil 
    :accessor subject
    :initarg :subject)
   (object
    :documentation "object go-term"
    ;;:initform nil 
    :accessor object
    :initarg :object)
   (relationship
    :documentation "relationship name string"
    ;;:initform nil 
    :accessor relationship
    :initarg :relationship)))

;;;
;;; Constructor wrappers.
;;;
;;; TODO: More or less a constructors for GO terms and
;;; relationships. Is it right to bootstrap in like this?
;;;

(defun create-go-term (engine pre-go-term)
  "Make a term object from an engine and an acc as string or a go-term object."
  (let ((cl-term
	 (if (stringp pre-go-term)
	     (get-term-by-acc engine pre-go-term)
	   pre-go-term)))
    (if cl-term
	(make-instance 'go-term
		       :term cl-term
		       :engine engine
		       :id (slot-value cl-term 'go-schema:acc)))))

(defun create-go-relation (engine subject-go-term object-go-term relationship)
  "Make a relation object."
  (make-instance 'go-relation
		 :engine engine
		 :subject subject-go-term
		 :object object-go-term
		 :relationship relationship))

;;;
;;; unique-key
;;;

(defgeneric unique-key (object)
  (:documentation "Returns a unque key for a GO object."))

;;
(defmethod unique-key ((go-term go-term))
  "Return a unique key (string) for a go-relationship."
  (with-slots
   (term) go-term
   (acc term)))

;;
(defmethod unique-key ((rel go-relation))
  "Return a unique key (string) for a go-relationship."
  (with-slots
   (subject object relationship) rel
   (concatenate 'string
		(unique-key subject)
		(name (car (relationship relationship))) ; TODO: too ugly
		(unique-key object))))

;;;
;;; info
;;;

(defgeneric info (object)
  (:documentation "Returns a list of important information about a GO object."))

;; Specialized on generic object.
(defmethod info ((obj go-object))
  "Get a little info on a GO object."
  (with-slots
   ;;(id type) obj
   ;;(list :id id :type type)))
   (id) obj
   (list :id id)))

;; Specialized on term over generic.
(defmethod info ((term go-term))
  "Get a little info on a GO term."
  (let ((obj-info (call-next-method)))
    (append obj-info (list :name (name (term term))
			   :term-type (term-type (term term))
			   :term (term term)))))

;; Specialized on relation over generic.
(defmethod info ((rel go-relation))
  "Get a little info on a GO relation."
  (let ((obj-info (call-next-method)))
    (append obj-info (list :relationship
			   (name (car (relationship (relationship rel))))
			   :subject (info (subject rel))
			   :object (info (object rel))))))

;;;
;;; print-info, a bit based on info
;;;

;; NOTE: nothing specialized on go-object.
(defgeneric print-info (object)
  (:documentation "Print important information about a GO object."))

;; ;; Specialized on term.
(defmethod print-info ((term go-term))
  "Print a little info on a GO term."
  (let ((plist (info term)))
    (loop
     for prop in (toolkit-conv:plist-keys plist)
     do (format t "~a: ~a~%" prop (getf plist prop)))))

;; ;; Specialized on object.
(defmethod print-info ((obj go-object))
  "Print a little info on a GO term."
  (let ((plist (info obj)))
    (loop
     for prop in (toolkit-conv:plist-keys plist)
     do (format t "~a: ~a~%" prop (getf plist prop)))))

;;;
;;; ...
;;;

(defun get-term-acc (go-term)
  "Return the acc string from a GO term."
  (acc (term go-term)))
  
(defun get-relationship-name (go-t2t)
  "Return the acc string from a GO term2term."
  (go-schema::name (car (slot-value go-t2t 'relationship))))

(defun children (in-go-term)
  "Return a list of the children of a term."
  (let ((engine (engine in-go-term))
	(parent-acc (get-term-acc in-go-term))
	(graph-paths
	 ;; Filter out non-immediate descendents. 
	 (remove-if #'(lambda (x) (not (eq 1 (slot-value x 'distance))))
		    (graph-child-relations (term in-go-term)))))
    (mapcar #'(lambda (x)
		(let ((go-term
		       (create-go-term engine
				       (slot-value x 'go-schema::subject))))
		  (create-go-relation engine
		   in-go-term
		   go-term
		   (get-relationship-by-accs engine
					     (get-term-acc go-term)
					     parent-acc))))
	    graph-paths)))

(defun parents (in-go-term)
  "Return a list of the parents of a term."
  (let ((engine (engine in-go-term))
	(parent-acc (get-term-acc in-go-term))
	(graph-paths
	 ;; Filter out non-immediate descendents. 
	 (remove-if #'(lambda (x) (not (eq 1 (slot-value x 'distance))))
		    (graph-parent-relations (term in-go-term)))))
    (mapcar #'(lambda (x)
		(let ((go-term
		       (create-go-term engine
				       (slot-value x 'go-schema::object))))
		  (create-go-relation engine
		   in-go-term
		   go-term
		   (get-relationship-by-accs engine
					     parent-acc
					     (get-term-acc go-term)))))
	    graph-paths)))

;; (defun descendants (term)
;;   "Return a list of the descendants of a term."
;;   term
;;   nil)

;;  (defun ancestors (term)
;;   "Return a list of the ancestors of a term."
;;   term
;;   nil)

(defun object-list-from-relation-list (rel-list)
  ""
  (mapcar #'(lambda (x) (object x))
	  rel-list))

(defun subject-list-from-relation-list (rel-list)
  ""
  (mapcar #'(lambda (x) (subject x))
	  rel-list))

(defun parent-relations-from-term-list (term-list)
  ""
  (mapcar #'(lambda (x) (parents x))
	  term-list))

;;
(defun up (go-term)
  "Takes a go-term and returns the relations up one step and the nodes
up one step."
  (let ((parent-relations (parents go-term)))
    (values (if parent-relations parent-relations)
	    (if parent-relations
		(mapcar #'(lambda (x) (object x)) parent-relations)))))

;; ;; TODO: This will be silly/hard until there is an edge model.
;; (defun relations-up (relations)
;;   "Takes a relation list and return a relation list for the next level up."
;;   (if relations
;;       (flatten
;;        (parent-relations-from-term-list
;; 	(object-list-from-relation-list relations)))))

;; ;; TODO: This will be silly/hard until there is an edge model.
;; ;; BUG: bootstrapping with this for now, need to flatten or use different mapX
;; (defun close-up (term_s)
;;   ""
;;   (let ((hash (make-hash-table :test #'equal)))
;;   (let ((relations (if (listp term_s)
;; 		       (mapcar #'(lambda (x) (parents x)) term_s)
;; 		     (parents term_s))))
;;     relations))

