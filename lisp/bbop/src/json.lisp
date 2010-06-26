;;;;
;;;; NOTE:
;;;;

(require :cl-json)

(defpackage :bbop-json
  (:use
   :cl)
  (:export
   :get-hash-val
   :json-to-hash))
(in-package :bbop-json)


;; (defun get-hash-val (hash key)
;;   (let ((new-key
;; 	 (intern (concatenate 'string "+" (symbol-name key) "+") "KEYWORD")))
;;     (gethash new-key hash)))

(defun json-to-hash (json-str)
  ""
  (let ((hash (make-hash-table)))
    (mapcar #'(lambda (x)
		(setf (gethash (car x) hash) (cdr x)))
	    (json:decode-json-from-string json-str))
    hash))
