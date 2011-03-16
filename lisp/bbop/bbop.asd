;;;; -*- mode: Lisp -*-
;;;;
;;;; A general set of utilities.
;;;;


(defpackage :bbop-asd
  (:use :cl :asdf))
(in-package :bbop-asd)

;; Tanuki
(defsystem bbop
  :name "Berkeley BOP Lisp utility belt."
  :version "0.2"
  :author "Seth Carbon <sjcarbon@berkeleybop.org>"
  :maintainer "Seth Carbon <sjcarbon@berkeleybop.org>"
  :licence "Modified BSD"
  :description "General purpose utility toolkit."
  :long-description "..."
  :components ((:static-file "bbop.asd")
               (:module :src
                        :serial t
                        :components ((:file "package")
				     (:file "obo")
				     (:file "couchdb"))))
                                     ;; (:file "list")
                                     ;; (:file "fs")
                                     ;; (:file "num")
                                     ;; (:file "util")
                                     ;; (:file "time")
                                     ;; (:file "log")
                                     ;; (:file "repl")
				     ;; (:file "obo")
                                     ;; (:file "shell"))))
				     ;; (:file "url")
				     ;; (:file "utils")
				     ;; (:file "json")
				     ;; (:file "couchdb")
  :depends-on (:cl-ppcre
	       :cl-json
	       :cl-fad
	       :local-time
	       :day-to-day
	       ;; :fiveam
	       ;; :puri
	       ;; :drakma
	       )
  ;; TODO: doc
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
