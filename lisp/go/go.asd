;;;; -*- lisp -*-

;;;;
;;;; GO-MODEL> (setf foo (make-instance 'engine))
;;;; GO-MODEL> (start-engine foo)
;;;; GO-MODEL> (info (create-go-term foo "GO:0000437"))
;;;; GO-MODEL> (children (create-go-term foo "GO:0000437"))
;;;; GO-MODEL> (parents (create-go-term foo "GO:0000437"))
;;;;
;;;; WISH: Change go-term printer to display acc.
;;;;
;;;; TODO: build-graph (takes a function that we can play with during the building process)
;;;; TODO: close-up
;;;;

(defpackage :go-asd
  (:use :cl :asdf))
(in-package :go-asd)

;; GO
(defsystem go
  :name "go"
  :version "0.1"
  :author "Seth Carbon <sjcarbon@berkeleybop.org>"
  :maintainer "Seth Carbon <sjcarbon@berkeleybop.org>"
  :licence "Modified BSD"
  :description "GO database access."
  :long-description "GOOOOOOOO database access."
  :components ((:static-file "go.asd")
               (:module :src
                        :serial t
                        :components (;;(:file "database")
				     ;;(:file "sutils")
				     (:file "schema")
				     (:file "engine")
				     (:file "model")
				     (:file "graph")
				     ;;(:file "web")
				     ;;(:file "shell")
				     )))
  :depends-on (:cl-graph
	       :cl-ppcre
	       :clsql
	       :clsql-mysql
	       :weblocks
	       :toolkit)
  ;; And some stuff for Albert...
  :properties ((#:author-email . "sjcarbon@berkeleybop.org")
	       (#:date . "Summer 2008")
	       ((#:albert #:output-dir) . "docs")
	       ((#:albert #:formats) . ("docbook"))
	       ((#:albert #:docbook #:template) . "book")
	       ((#:albert #:docbook #:bgcolor) . "white")
	       ((#:albert #:docbook #:textcolor) . "black")))
