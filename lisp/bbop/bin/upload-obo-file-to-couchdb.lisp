
(require :bbop)
(defpackage :bbop-bin
  (:use
   :cl
   :bbop-utils
   :bbop-json
   :bbop-couchdb
   :bbop-obo))
(in-package :bbop-bin)

(defparameter +file-loc+
  "/home/sjcarbon/local/src/cvs/go/ontology/gene_ontology.obo")
(defparameter +db-name+
  "geneontology")

(defun start ()
  (create-db +db-name+)
  (loop for record in (parse-file +file-loc+)
	do (let* ((hash (generate-flat-hash record))
		  (id (gethash :id hash)))
	     (if id
		 (add-doc +db-name+ id hash)))))

