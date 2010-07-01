;;;; -*- mode: Lisp -*-
;;;;
;;;; Will be used to generate input and form data for tanuki fuzzing.
;;;; Ran across this while on Norvig's site--need to reread that
;;;; book. Will rewrite for our purposes. See:
;;;; http://norvig.com/python-lisp.html
;;;;
;;;; > (generate 'S)
;;;;
;;;; TODO/BUG: make this work
;;;;

;;(require :puri)
(defpackage :tanuki-grammar
  (:use :cl)
  ;;	:drakma
  ;;	:puri)
  (:export foo))
;; fetch
;; purge
;; tanuki-agent))
(in-package :tanuki-grammar)

;;
(defun foo () 'foo)

(defparameter *grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

