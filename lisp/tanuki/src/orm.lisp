;;;; -*- mode: Lisp -*-
;;;;
;;;; ORMish.
;;;;
;;;; TODO: Simplify the one-to-many joins with a macro.
;;;;

(defpackage :tanuki-orm
  (:use :cl
	:postmodern)
  (:export
   ;; Parse.
   :parse-page
   :parse-argument-set
   :parse-hit
   :parse-comment
   ;; Join.
   :comment->hit
   :hit->argument-set
   :argument-set->page
   :page=>argument-set
   :argument-set=>hit
   :hit=>comment
   ;; Remove.
   :remove-comment
   :remove-hit
   :remove-argument-set
   :remove-page
   ))
(in-package :tanuki-orm)

;;;
;;; Parse to DAO.
;;;

(defun parse-page (page?)
  "Try and turn something into a page dao."
  (cond
   ((eq 'tanuki-schema:page (class-name (class-of page?)))
    page?)
   ((integerp page?)
    (car (select-dao 'tanuki-schema:page (:= 'tanuki-schema:id page?))))
   ((stringp page?)
    (car (select-dao 'tanuki-schema:page (:= 'tanuki-schema:url page?))))
   (t
    (error "type unsupported by parse-page"))))

(defun parse-argument-set (aset? &optional (ptype 'clean-url))
  "Try and turn something into an argument set dao. Opt: 'clean-url 'raw-url"
  (cond
   ((eq 'tanuki-schema:argument-set (class-name (class-of aset?)))
    aset?)
   ((integerp aset?)
    (get-dao 'tanuki-schema:argument-set aset?))
   ((and (stringp aset?) (eq ptype 'clean-url))
    (car (select-dao 'tanuki-schema:argument-set
                     (:= 'tanuki-schema:clean-url aset?))))
   ((and (stringp aset?) (eq ptype 'raw-url))
    (car (select-dao 'tanuki-schema:argument-set
                     (:= 'tanuki-schema:raw-url aset?))))
   ;; ;; Not a 'raw-url or clean-url so try both, starting with clean.
   ;; ((stringp aset?)
   ;;  (let ((clean-try (parse-argument-set aset? 'clean-url)))
   ;;    (if clean-try
   ;;        clean-try
   ;;      (parse-argument-set aset? 'raw-url))))
   (t
    (error "type unsupported by parse-argument-set"))))

(defun parse-hit (hit?)
  "Try and turn something into a hit dao."
  (cond
   ((eq 'tanuki-schema:hit (class-name (class-of hit?)))
    hit?)
   ((integerp hit?)
    (car (select-dao 'tanuki-schema:hit (:= 'tanuki-schema:id hit?))))
   (t
    (error "type unsupported by parse-hit"))))

(defun parse-comment (comment?)
  "Try and turn something into a hit comment."
  (cond
   ((eq 'tanuki-schema:comment (class-name (class-of comment?)))
    comment?)
   ((integerp comment?)
    (car (select-dao 'tanuki-schema:comment (:= 'tanuki-schema:id comment?))))
   (t
    (error "type unsupported by parse-comment"))))

;;;
;;; Unexported join helpers.
;;;

(defun make-id (symbol)
  (intern (concatenate 'string (symbol-name symbol) "-ID")))

;; TODO:
(defmacro simple-single-join (in-arg join-to)
  "..."
  `(let ((tmp-id (,(make-id join-to) ,in-arg)))
    (get-dao ',join-to tmp-id)))

;;;
;;; Relational movement.
;;;

(defun comment->hit (comment)
  "A comment to a hit."
  (let ((tmp-id (tanuki-schema:hit-id comment)))
    (get-dao 'tanuki-schema:hit tmp-id)))

(defun hit->argument-set (hit)
  "A hit to an argument_set."
  (let ((aset-id (tanuki-schema:argument-set-id hit)))
    (get-dao 'tanuki-schema:argument-set aset-id)))

(defun argument-set->page (argument-set)
  "An argument_set to a page."
  (let ((page-id (tanuki-schema:page-id argument-set)))
    (get-dao 'tanuki-schema:page page-id)))

(defun page=>argument-set (page)
  "An page to argument_set(s)."
  (when page
    (query-dao 'tanuki-schema:argument-set
	       (:select 'argument-set.* :from 'argument-set
			:inner-join 'page
			:on (:= 'argument-set.page-id 'page.id)
			:where (:= 'page.id (tanuki-schema:id page))))))

(defun argument-set=>hit (aset)
  "An argument_set to hit(s)."
  (when (and aset (tanuki-schema:id aset))
    (query-dao 'tanuki-schema:hit
               (:select 'hit.* :from 'hit
                        :inner-join 'argument-set
                        :on (:= 'hit.argument-set-id 'argument-set.id)
                        :where (:= 'argument-set.id
                                   (tanuki-schema:id aset))))))

(defun hit=>comment (hit)
  "An hit to comment(s)."
  (when (and hit (tanuki-schema:id hit))
    (query-dao 'tanuki-schema:comment
               (:select 'comment.* :from 'comment
                        :inner-join 'hit
                        :on (:= 'comment.hit-id 'hit.id)
                        :where (:= 'hit.id
                                   (tanuki-schema:id hit))))))

;;;
;;; Transitive removal.
;;;

(defun remove-comment (comment?)
  "Remove a comment from the database."
  (let ((comment (parse-comment comment?)))
    (when comment
      (delete-dao comment))))

(defun remove-hit (hit?)
  "Remove a hit from the database."
  (let ((hit (parse-hit hit?)))
    (when hit 
      (with-transaction ()
       (dolist (comment (hit=>comment hit))
         (remove-comment comment))
      (delete-dao hit)))))
       
(defun remove-argument-set (aset?)
  "Remove an argument set and all of its related hits."
  (let ((aset (parse-argument-set aset?)))
    (when aset
      (with-transaction ()
       (dolist (hit (argument-set=>hit aset))
         (remove-hit hit))
       (delete-dao aset)))))

(defun remove-page (page?)
  "Remove page and its associated argument sets and related hits."
  (let ((page (parse-page page?)))
    (when page
      (with-transaction ()
       (dolist (aset (page=>argument-set page))
         (remove-argument-set aset))
       (delete-dao page)))))
