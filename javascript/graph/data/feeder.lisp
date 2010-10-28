;;;; -*- mode: Lisp -*-
;;;;
;;;; (connect)
;;;; (dump-graph-to-json "data/go.js")
;;;;

(ql:quickload "clsql-mysql")
(ql:quickload "cl-json")
(ql:quickload "metabang-bind")
;;(require :clsql)
;;(require :clsql)

(defpackage :feeder
  (:use :cl
	:metabang.bind))
(in-package :feeder)

(defparameter +connection-bits+
  `("localhost" "go_latest_lite" "sjcarbon" ""))

(defun connect ()
  (clsql-sys:connect +connection-bits+ :database-type :mysql))

(defun property-merge (plist vlist)
  (cond
    ((null plist) nil)
    (t (append (list (car plist) (car vlist))
	       (property-merge (cdr plist) (cdr vlist))))))

(defun association-merge (plist vlist)
  (cond
    ((null plist) nil)
    (t (cons (cons (car plist) (car vlist))
	     (association-merge (cdr plist) (cdr vlist))))))

(defun run-sql (sql)
  (bind (((:values nodes layout)
	  (clsql-sys:query sql))
	 (keywords (mapcar (lambda (x)
			     (intern (string-upcase x) "KEYWORD"))
			   layout)))
    (mapcar (lambda (row) (association-merge keywords row)) nodes)))

(defun get-nodes ()
  (run-sql "SELECT acc AS id, name AS label FROM term WHERE is_obsolete = 0 AND acc != 'all' AND (term_type ='molecular_function' OR term_type ='biological_process' OR term_type ='cellular_component')"))

(defun get-edges ()
  (run-sql "SELECT DISTINCT sub.acc AS subject, obj.acc AS object, rel.acc AS predicate FROM graph_path, term AS sub, term AS obj, term AS rel WHERE graph_path.term2_id = sub.id AND graph_path.term1_id = obj.id AND graph_path.relationship_type_id = rel.id AND obj.acc != 'all' AND distance = 1"))

(defun get-field (row field)
  (getf row field))

(defun dump-graph-to-json (file)
  ;; (bind ((nodes (get-nodes))
  ;; 	   (edges (get-edges))
  ;; 	   (all (json:encode-json-to-string (list (cons :nodes nodes)
  ;; 						  (cons :edges edges)))))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (format stream "bbop.model.go = {};~%")
    (format stream "bbop.model.go.nodes = ")
    (format stream "~A" (json:encode-json-to-string (get-nodes)))
    (format stream ";~%")
    (format stream "bbop.model.go.edges = ")
    (format stream "~A" (json:encode-json-to-string (get-edges)))
    (format stream ";~%")
    t))
