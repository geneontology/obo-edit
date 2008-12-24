;;;;
;;;; TODO: merge the clsql schema model and the go-model better to
;;;; make things work a little more sanely with weblocks (and life in
;;;; general).
;;;;

(require 'toolkit)
(require 'go)
(require 'weblocks)
;;(require 'weblocks)
(defpackage :go-web
  (:use :cl
	:toolkit
	:weblocks
	:go-model))
(in-package :go-web)


;;;
;;; Data views for weblocks.
;;;

;; (defview card-table-view (:type table :inherit-from '(:scaffold card))
;;   (id :hidep t)
;;   kanji
;;   yomikata
;;   explanation)

(defview term-data-view (:type data :inherit-from '(:scaffold ))
  (id :hidep t))

;;(defview card--view (:type data :inherit-from '(:scaffold card))
;;  kanji)

;;;
;;; Weblocks control from command line.
;;;
;; (load "./kblock.lisp")
;; (in-package :kblock)
;; (sb-ext:save-lisp-and-die "/tmp/kblock" :executable t :toplevel 'kblock :purify t))
;; on olpc, libssl is in "/lib"
(defun kblock ()
  "Get it up and working again."
  ;;(weblocks:stop-weblocks)
  (weblocks:start-weblocks :debug t)
  (weblocks:reset-sessions)
  (sleep 9999999) ; too big in some cases...
  1)

;;;
;;; Applications...
;;;

;;(weblocks:defwebapp our-application :prefix "/")
(weblocks:defwebapp 'our-app)

;; Called once at the beginning per user per session. Basically, init
;; data and pass on to the first page.
(defun init-user-session (comp)
  ""
  (let ((file-string "/home/sjcarbon/local/src/svn/home/kserve/test.txt"))

    ;; Set out the first page.
    (setf (weblocks:composite-widgets comp)
	  (start-page comp (data-to-model (get-data file-string))))))

;;(let ((foo 0))
(defun start-page (comp card-list)
  ""
  (setf (weblocks:composite-widgets comp)
	(list ;(with-html (:div (:p "foo")))
	   (lambda (&rest args)
	     ;;	   (setf foo (1+ foo))
	     (weblocks:render-link (lambda (&rest args)
				     ;;(declare (ignore args))
				     (review-page comp (toolkit-num:shuffle card-list) 1))
				   (toolkit-conv:ccat "Show review (" (write-to-string (length card-list)) " cards)")))
	   (lambda (&rest args)
	     ;;	   (setf foo (1+ foo))
	     (weblocks:render-link (lambda (&rest args)
				     ;;(declare (ignore args))
				     (question-page comp (toolkit-num:shuffle card-list) 'explanation 1))
				   (concatenate 'string "Start manual (" (write-to-string (length card-list)) " cards)"))))))
;;)

;;
(defun review-page (comp card-list index)
  (setf (weblocks:composite-widgets comp)
	(list
	 (weblocks:render-object-view card-list 'card-table-view)
	   (weblocks:render-link (lambda (&rest args)
				   ;;(declare (ignore args))
				   (start-page comp card-list))
				 "top"))))

;;
(defun question-page (comp card-list slot index)
  (setf (weblocks:composite-widgets comp)
	(list
	 (lambda (&rest args)
	   ;;(declare (ignore args))
	   (weblocks:render-link (lambda (&rest args)
				   ;;(declare (ignore args))
				   (answer-page comp card-list slot index))
				 "answer"))
	 (slot-value (elt card-list (1- index)) 'explanation)
	 (with-html (:div (:p (write-to-string index)))))))

;;
(defun answer-page (comp card-list slot index)
  ""
  (setf (weblocks:composite-widgets comp)
	(list
	 ;(with-html (:div (:p (write-to-string index))))
	 ;; 
	 (lambda (&rest args)
	   ;;(declare (ignore args))
	   (weblocks:render-link (lambda (&rest args)
				   ;;(declare (ignore args))
				   (question-page comp card-list slot (1+ index)))
				 "ok"))
	 (lambda (&rest args)
	   ;;(declare (ignore args))
	   (weblocks:render-link (lambda (&rest args)
				   ;;(declare (ignore args))
				   (start-page comp card-list))
				 "top"))
	 (weblocks:render-object-view (elt card-list (1- index))
				      'card-data-view))))
