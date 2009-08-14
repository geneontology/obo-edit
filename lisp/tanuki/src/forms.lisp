;;;;
;;;;
;;;;

(require :toolkit)
(defpackage :tanuki-forms
  (:use :cl))
;;  (:export))
(in-package :tanuki-forms)

;;;
;;; Probabilistic deciders
;;;
;;; and: return all as list
;;; or: return one in prop
;;; maybe: return nil or form
;;; subset: 
;;;

;; (defun p/or (&rest body-list)
;;   ""
;;   (nth (random (length body-list)) body-list))

;; (defun p/and (&rest body-list)
;;   ""
;;   body-list)

;; (defun p/maybe (stmnt)
;;   ""
;;   (if (evenp (random 2)) stmnt))

;; ;; (defun p/subset (&rest body-list)
;; ;;   ""
;; ;;   )

;; ;;;
;; ;;; Functions to pull from the filesystems
;; ;;;

;; (defun f/random-file-from-directory (dir)
;;     ""
;;     (nth (random (length (toolkit-fs:list-files dir)))
;; 	 (toolkit-fs:list-files dir)))

;; ;;;
;; ;;; Fuzz generators. 
;; ;;;

;; ;; TODO: use toolkit-num

