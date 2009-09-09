;;;;
;;;; Working towards an independent kappa.
;;;;

(require :bbop)
(require :cl-fad)
(require :tanuki)
(defpackage :kappa
  (:use :cl
	:drakma)
  (:export 
   ))
(in-package :kappa)


(defun attempt-get-1 ()
  (http-request "http://localhost/cgi-bin/amigo/form_test?mode=simple"
		:method :get
		:parameters '(("one" . "bar")
			      ("two" . "bibble"))))

;; As it should.
(defun attempt-get-2 ()
  (http-request "http://localhost/cgi-bin/amigo/form_test?mode=simple"
		:method :get
		:parameters '(("one" . "bar")
			      ("file" . #p"/tmp/test.txt")
			      ("two" . "bibble"))))

(defun attempt-post ()

  (http-request "http://localhost/cgi-bin/amigo/form_test?mode=simple"
		:method :post
		:content-length t
		:parameters '(
;; 			      ("file" #p"/tmp/test.txt"
;; 			       :content-type "plain/text"
;; 			       :filename "myfun.txt")
			      ("file" . #p"/tmp/test.txt")
			      ("mode" . "simple")
			      ("one" . "foo")
			      ("two" . "bibble")))
)


(defparameter +data-directory+
  "/home/sjcarbon/local/src/svn/geneontology/lisp/tanuki/data/")

;; TODO: do existance check...
(defun data-path (file-str)
  (cl-fad:pathname-as-file
   (bbop-utils:string-merge +data-directory+ file-str)))

;; TODO: Autogenerate these using grammar.
(defparameter +set-1+
  `(("gp_file_type" . "list")
    ("gp_file" . ,(data-path "gp/sgd-small-10.txt"))
    ("bggp_file_type" . "list")
    ("bggp_file" . ,(data-path "gp/sgd-small-40.txt"))
    ("request" . "results")
    ("force" . "yes")
    ("speciesdb" . "SGD")
    ("min_gps" . "2")
    ("cutoff" . "10")
    ("output" . "normal")))

(defun attempt-te (set)
  (http-request "http://localhost/cgi-bin/amigo/term_enrichment"
		:method :post
		:content-length t
		:parameters set))

;; TODO
(defun exhaust-page (html-string)
  "Make sure that every URL on the page is okay."
  (let ((page-truth (mapcar #'(lambda (puri)
				(tanuki-html:fetch-doc
				 (puri:render-uri puri nil)))
			    (tanuki-html::merge-with-base
			     (tanuki-html::pull-all-hrefs html-string)
			     "http://localhost/cgi-bin/amigo/"))))
    page-truth))

(defun url-link-html (url-str)
  (nth-value 0 (http-request url-str)))

;;(setf resp (attempt-te +set-1+))
;;(setf nl (subseq (puri:render-uri (car (tanuki-html::merge-with-base (list (nth 25 (tanuki-html::pull-all-hrefs resp))) "http://localhost/cgi-bin/amigo/")) nil) 79 80))
;;(http-request (cl-ppcre:regex-replace-all nl (puri:render-uri (car (tanuki-html::merge-with-base (list (nth 25 (tanuki-html::pull-all-hrefs resp))) "http://localhost/cgi-bin/amigo/")) nil) ""))

;;;
;;; Language functions.
;;;

(defun p/or (&rest body-list)
  (nth (random (length body-list)) body-list))

(defun p/and (&rest body-list)
  body-list)

(defun p/optional (stmnt)
  (if (evenp (random 2)) stmnt nil))

;;(defun fuzzy

;;(p/and (p/or 1 2) (p/or 3 4) )


;; TODO: file agent--once on dir, can to things like get random file.


;;;
;;;
;;;



;;(defparameter +states+ '("a" "b"))
;; A FSM should be fully definable by the transitions.
(defparameter +transitions+ '((("a" . "a") .5)
			      (("a" . "b") .5)
			      (("b" . "a") .9)
			      (("b" . "b") .1)))
(defparameter +current-state+ "a")

(defparameter +precision+ 1000000)
(defun between-1-and-0 ()
  (/ (random +precision+) +precision+))


;;; Making use of defaults.
;;; > (setf f (make-instance 'fsm))
;;; > (add-transtate f t (make-transtate-function (t "entering" 'a)))
;;; > (add-transtate f 'a (make-transtate-function (t "foo" 'b)))
;;; > (add-transtate f 'b (make-transtate-function (t "bar" 'a)))
;;; > (current-state f)
;;; > (next f)
;;; > (next f)
;;; > (next f)
;;; > (next f)
;;;

;;; Making use of complete.
;;; > (setf f (make-instance 'fsm))
;;; > (add-transtate f t (make-transtate-function (t "entering" 'a)))
;;; > (add-transtate f 'a (make-transtate-function (t "foo" 'b)))
;;; > (add-transtate f 'b (make-transtate-function (t "bar" nil)))
;;; > (complete f)
;;;

;;; Simple use of functions.
;;; > (setf f (make-instance 'fsm))
;;; > (add-transtate f t (make-transtate-function (t "entering" 'a)))
;;; > (add-transtate f 'a (make-transtate-function (t "foo" 'b)))
;;; > (add-transtate f 'b (make-transtate-function (t "bar" nil)))
;;; > (complete f)
;;;

(defclass fsm ()
  ((current-state
    :documentation "The current state (id) of the FSM. The default is t"
    :initform t
    :accessor current-state)
   (lookup-table
    :documentation "The lookup hash for the FSM transition function."
    :initform (make-hash-table)
    :accessor lookup-table)))

(defgeneric change-state (fsm new-state)
  (:documentation "Changes the current state of the FSM."))

(defmethod change-state ((fsm fsm) new-state)
  (setf (current-state fsm) new-state))

;;
(defgeneric add-transtate (fsm t-state t-function)
  (:documentation "Adds a state shifting function to an FSM."))

(defmethod add-transtate ((fsm fsm) t-state t-fun) 
  (with-slots (current-state lookup-table) fsm
    (setf (gethash t-state lookup-table) t-fun)))

;; Note: unnecessary?
(defgeneric reset (fsm)
  (:documentation "Resets the FSM to its original state."))

(defmethod reset ((fsm fsm)) 
  (change-state fsm t))

(defgeneric look-ahead (fsm &key input)
  (:documentation "Returns the next state and the AV. Doesn't change state."))

(defmethod look-ahead ((fsm fsm) &key (input t))
    (with-slots (current-state lookup-table) fsm
      ;;(format t "current-state: ~A~%" current-state) 
      ;;(format t "current-state: ~A~%" current-state) 
      (multiple-value-bind (lookup-function lookup-function?)
	  (gethash current-state lookup-table)
	(if lookup-function?
	    (multiple-value-bind (transition-object next-state)
		(funcall lookup-function input)
	      (values transition-object next-state))
	    (values nil nil)))))

(defgeneric next (fsm &key input)
  (:documentation "Moves the FSM into its next state and returns object."))

(defmethod next ((fsm fsm) &key (input t))
  (multiple-value-bind (transition-object next-state)
      (look-ahead fsm :input input)
    (change-state fsm next-state)
    transition-object))

(defgeneric complete (fsm &key input)
  (:documentation "Runs FSM until nil is reached as a state. Returns list of output objects."))

(defmethod complete ((fsm fsm) &key (input t))
  (cond
    ((null (current-state fsm)) nil)
    (t (cons (next fsm :input input) (complete fsm :input input)))))

(defun eq? (a b)
  (cond
    ((and (stringp a) (stringp b)) (string= a b))
    (t (equalp a b))))

;; ((<input> <return object> <new state>) ...)
(defmacro make-transtate-function (&rest ts-list)
  (let ((inner (mapcar #'(lambda (ts-triple)
			   (let ((input-object (elt ts-triple 0))
				 (output-object (elt ts-triple 1))
				 (next-state-object (elt ts-triple 2)))
			     `((eq? input ,input-object)
			     (values ,output-object
				     ,next-state-object))))
		       ts-list)))
    `(lambda (input)
       (cond
	 ,@inner
	 (t (values nil nil))))))

;;;
;;;
;;;

;; (defun generate-go-id-2 ()
;;   (let ((f (make-instance 'fsm))
;; 	(rand (lambda () (random 10))))
;;     (add-transtate f 1 (make-transtate-function (t "foo" 2)))
;;     (add-transtate f 2 (make-transtate-function (t "foo" 2)))
;;     (add-transtate f 3 (make-transtate-function (t "foo" 2)))
;;     (add-transtate f 4 (make-transtate-function (t "foo" 2)))
;;     (add-transtate f 'a (make-transtate-function (t "foo" 'b)))

(defun generate-go-id ()
  (bbop-utils:join-strings
   (cons "GO:"
   (mapcar #'write-to-string (loop for x from 1 to 7
				for y = (random 10)
				collect y)))))
