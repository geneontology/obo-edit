;;;; -*- lisp -*-
;;;;
;;;;
;;;;


(defpackage :bbop-asd
  (:use :cl :asdf))
(in-package :bbop-asd)

;; Tanuki
(defsystem bbop
  :name "bbop"
  :version "0.1"
  :author "Seth Carbon <sjcarbon@berkeleybop.org>"
  :maintainer "Seth Carbon <sjcarbon@berkeleybop.org>"
  :licence "Modified BSD"
  :description "BBOP utilities, thoughts, and tools."
  :long-description "Blah, blah, blah."
  :components ((:static-file "bbop.asd")
               (:module :src
                        :serial t
                        :components (
				     ;;(:file "url")
				     (:file "utils")
				     (:file "json")
				     (:file "couchdb")
				     (:file "obo")
				     )))
  :depends-on (:cl-ppcre
	       :cl-json
	       :cl-fad
	       :fiveam
;;	       :puri
	       :drakma)
;;   ;; And some stuff for Albert...
;;   :properties ((#:author-email . "sjcarbon@berkeleybop.org")
;; 	       (#:date . "Spring 2008")
;; 	       ((#:albert #:output-dir) . "docs")
;; 	       ((#:albert #:formats) . ("docbook"))
;; 	       ((#:albert #:docbook #:template) . "book")
;; 	       ((#:albert #:docbook #:bgcolor) . "white")
;; 	       ((#:albert #:docbook #:textcolor) . "black")))
)

;; ;; Add tanuki to our features.
;; (defmethod asdf:perform
;;     :after ((op asdf:load-op)
;; 	    (system (eql (asdf:find-system :tanuki))))
;;   (pushnew :tanuki cl:*features*))

;; (defmethod asdf:perform ((o test-op) (c (eql (find-system :tanuki))))
;;   (asdf:operate 'asdf:test-op :tanuki-test))

;; ;; Describe the testing system.
;; (defsystem :tanuki-test
;;   :components ((:module :t
;;                         :serial t
;;                         :components ((:file "tanuki-test"))))
;;   :depends-on (:tanuki :fiveam))

;; ;; TODO: Not sure exactly what this magical incanation does, no doubt
;; ;; running tests on load of tanuki.
;; (defmethod asdf:perform ((o test-op) (c (eql (find-system :tanuki-test))))
;;   (asdf:operate 'asdf:load-op :tanuki-test)
;;   (funcall (intern (symbol-name :run-tests)
;;                    (find-package :tanuki-test))))
