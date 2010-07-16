;;;; -*- mode: Lisp -*-
;;;;
;;;; 
;;;;


(defpackage :mechanize-asd
  (:use :cl :asdf))
(in-package :mechanize-asd)

;;
(defsystem mechanize
  :name "Common Lisp Mechanize."
  :version "0.1.0"
  :author "Seth Carbon <sjcarbon@berkeleybop.org>"
  :maintainer "Seth Carbon <sjcarbon@berkeleybop.org>"
  :licence "Modified BSD"
  :description "General purpose web agent inspired by WWW:Mechanize."
  :long-description "General purpose web agent inspired by WWW:Mechanize."
  :components ((:static-file "mechanize.asd")
               (:module :src
                        :serial t
                        :components ((:file "mechanize"))))
  :depends-on (:cl-ppcre
	       :closure-html
	       :puri
	       :drakma
	       :trivial-timeout
	       :alexandria
	       :fiveam
	       )

  ;; TODO: Auto doc info...
)

;; Add mechanize to our features.
(defmethod asdf:perform :after ((op asdf:load-op)
				(system (eql (asdf:find-system :mechanize))))
  (pushnew :mechanize cl:*features*))

;; (defmethod asdf:perform ((o test-op) (c (eql (find-system :mechanize))))
;;   (asdf:operate 'asdf:test-op :mechanize-test))

;; Describe the testing system.
(defsystem :mechanize-test
  :components ((:module :t
                        :serial t
                        :components ((:file "mechanize-test"))))
  :depends-on (:mechanize
	       :fiveam
	       :hunchentoot))

;; ;; TODO: Not sure exactly what this magical incanation does, no doubt
;; ;; running tests on load of mechanize.
;; (defmethod asdf:perform ((o test-op) (c (eql (find-system :mechanize-test))))
;;   (asdf:operate 'asdf:load-op :mechanize-test)
;;   (funcall (intern (symbol-name :run-tests)
;;                    (find-package :mechanize-test))))
