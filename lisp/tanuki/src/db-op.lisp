;;;; -*- mode: Lisp -*-
;;;;
;;;; Handle database queries and operations.
;;;;
;;;; WARNING: Everything in here must we wrapped somehow in
;;;; with-connection from postmodern to work properly. We're
;;;; implicitly relying on *database* being defined to handle our
;;;; operations.
;;;;

(defpackage :tanuki-db-op
  (:use :cl
	:postmodern
	:tanuki-schema)
  (:export
   :row-count
   :table-plist
   :update-dao-value
   ;;:dbq
   ))
(in-package :tanuki-db-op)


(defun count-where (table-symbol &optional (where-list nil))
  "For macro use. Usage: (count-where 'meta '(:= 'data \"/data\"))"
  (let ((base-query `(:select (:count '*) :from ,table-symbol)))
    (when where-list
      (setf base-query (append base-query (list :where) (list where-list))))
    base-query))

(defun select-where (table-symbol &optional (where-list nil))
  "For macro use. Usage: (select-where 'meta '(:= 'data \"/data\"))"
  (let ((base-query `(:select '* :from ,table-symbol)))
    (when where-list
      (setf base-query (append base-query (list :where) (list where-list))))
    base-query))

;; (defmacro table-plist (tdb table-symbol &optional (where-list nil))
;;   "Return all rows in a table as a plist."
;;   `(with-connection
;;     (tanuki-db:parameters ,tdb)
(defmacro table-plist (table-symbol &optional (where-list nil))
  "Return all rows in a table as a plist."
  `(query ,(select-where table-symbol where-list) :plists))

;; (defmacro row-count (tdb table-symbol &optional (where-list nil))
;;   "Simplify getting usable simple row counts from tables. Int or nil."
;;   `(with-connection
;;     (tanuki-db:parameters ,tdb)
(defmacro row-count (table-symbol &optional (where-list nil))
  "Simplify getting usable simple row counts from tables. Int or nil."
  `(query ,(count-where table-symbol where-list) :single))

;; (defmacro dbq (tdb in-query &optional (flag nil))
;;   "Simplify doing general-purpose calls to the database."
;;   (let ((base-qcall `(query ,in-query)))
;;     (when flag
;;       (setf base-qcall (append base-qcall (list flag))))
;;     `(with-connection
;;       (tanuki-db:parameters ,tdb)
;;       ,base-qcall)))

;; (defmacro dbq2 (in-query &optional (flag nil))
;;   "Simplify doing general-purpose calls to the database."
;;   (let ((base-qcall `(query ,in-query)))
;;     (when flag
;;       (setf base-qcall (append base-qcall (list flag))))
;;     `,base-qcall))

(defun update-dao-value (dao slot-name slot-value)
  (setf (slot-value dao slot-name) slot-value)
  (update-dao dao))
