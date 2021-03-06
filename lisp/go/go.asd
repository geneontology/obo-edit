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
  :description "GO database access and GO related utilities."
  :long-description "GOOOOOOOO database access."
  :components ((:static-file "go.asd")
               (:module :common
			:pathname "src/common/"
                        :serial t
                        :components ((:file "packages")
				     ;;(:file "couchdb")
				     ;;(:file "irc")
				     (:file "obo")))
               (:module :lead
			:pathname "src/lead/"
                        :serial t
                        :components (
				     ;;((:file "schema")
				     ;;(:file "engine")
				     ;;(:file "model")
				     ))
	       (:module :godot
			:pathname "src/godot/"
                        :serial t
                        :components (
				     ;;(:file "godot")
				     ;;(:file "config")
				     ;;(:file "amigo-js")
				     ))
	       (:module :gold
			:pathname "src/gold/"
                        :serial t
                        :components (
				     ;; TODO
				     )))
  :depends-on (;:cl-graph
	       :cl-ppcre
	       ;:clsql
	       ;:clsql-mysql
	       :day-to-day))
