;;;; -*- mode: Lisp -*-
;;;;
;;;; TODO: Add and use "arguments" table keyed to page--fixes future
;;;; problems now.
;;;;
;;;; TODO/BUG: don't re-add links that are already in, just their
;;;; arguments.
;;;;
;;;; TODO/BUG: it looks like I misaligned some argument and eventually
;;;; a single url takes over the random...
;;;;
;;;; BUG: CLSQL (and so our sqlite3 store) seems to be hosed right
;;;; now, possibly due to uffi something or other, so we need to
;;;; switch. Even though I really liked cl-prevalence (memory +
;;;; serialization), easy relational functionality is necessary so
;;;; I'll go with postmodern.
;;;;
;;;; Usage: (require 'tanuki)
;;;;        (in-package :tanuki)
;;;;        (tdb-clean-slate "http://localhost/cgi-bin/amigo/amigo")
;;;;        (setf a (make-instance 'tanuki-agent))
;;;;        (agent-process-page a (random-unvisited-internal-page))
;;;;
;;;; TODO: Make the fail/odd system also report a "comment" about the
;;;; problem type.
;;;;
;;;; TODO: Logging.
;;;;
;;;; TODO: multiple target selection--internal, external, etc.
;;;;
;;;; TODO: be able to convert fails to mandates.
;;;;
;;;; TODO: Use objects for top level (should also help threading problems)
;;;;
;;;; TODO: form detection, then profiling, and then mandating.
;;;;
;;;; BUG:
;;;; Can we catch the GD mem error? sbcl --noinform --dynamic-space-size 1024
;;;; (- SB-VM:DYNAMIC-SPACE-END SB-VM:DYNAMIC-SPACE-START)
;;;; http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&Submit=Go&id=CP000158
;;;; Looks like the parsing in drakma is croaking. But the page must be OK?
;;;; But that solution only works when working with externals.
;;;;

(defpackage :tanuki
  (:use :cl
	;;:gs
	:postmodern
	;;:cl-prevalence
	;:toolkit
	;:tanuki-utils
	:tanuki-agent ; TODO/BUG: move agent to bweb, this should be subclass
	;:tanuki-decide
	;:tanuki-file
	;;:tanuki-web
	;:tanuki-db
	))
(in-package :tanuki)

;; ;;
;; (defparameter +tanuki-store+ "/home/sjcarbon/tmp/tanuki/foo1"
;;   "Location of the store.")
;; (defparameter +tanuki-data+ "/home/sjcarbon/local/src/svn/geneontology/lisp/tanuki/data"
;;   "Location of data to be used for forms.")
;; (defparameter +sample-size+ 33 ; down from 100
;;   "The size of the random sample in the database for each iteration.")
;; (defparameter +stop-signal+ nil
;;   "A flag to indicate that we should end after the current operation.")
;; (defparameter +tanuki-thread+ nil
;;   "The keeper of the running tanuki.")

;;;
;;; High-level database handling.
;;;

;; (dao-table-definition 'meta) looks correct...
(defclass meta ()
  ((start
    :accessor start
    :col-type bigint
    :initarg :start)
   (target
    :accessor target
    :col-type string
    :initarg :target))
  (:metaclass dao-class))
   
(defclass page ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (url
    :accessor url
    :col-type string
    :initarg :url)
   (internal
    :accessor internal
    :col-type bigint
    :initarg :internal)
   (mandated
    :accessor mandated
    :col-type bigint
    :col-default 0)
   (visited
    :accessor visited
    :col-type bigint
    :col-default 0))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'arguments) looks correct...
(defclass arguments ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (page-id
    :accessor page-id
    :col-type bigint
    :initarg :page-id)
   (arguments ;; TODO: this will be handy...
    :accessor arguments
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :arguments))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'hit) looks correct...
(defclass hit ()
  ((id
    :accessor id
    :col-type bigint
    :initarg :id)
   (arguments-id
    :accessor arguments-id
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :arguments-id)   
   (referer
    :accessor referer
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :referer)
   (wait
    :accessor wait
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :wait
    :documentation "Hopefully the time to completion for a request.")
   (date
    :accessor date
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :date
    :documentation "The approximate data/time of the hit.")
   (agent
    :accessor agent
    :col-type (or db-null string)
    :col-default :null
    :initform :null
    :initarg :agent
    :documentation "Token id for an invididual agent.")
   (flagged
    :accessor flagged
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :flagged)
   (success
    :accessor success
    :col-type (or db-null bigint)
    :col-default :null
    :initform :null
    :initarg :success))
  (:metaclass dao-class)
  (:keys id))

(defvar *database-tables* '(meta page arguments hit)
  "All the tables that are used in Tanuki's database.")
(defvar *database-sequences* '(page-id-seq arguments-id-seq hit-id-seq)
  "All the sequences that are used in Tanuki's database.")

(defparameter +default-target+ "http://localhost/cgi-bin/amigo/amigo"
  "The default target used when not explicitly defined.")

;; TODO: make flexible on connection values.
(defun tdb-connect ()
  "Make sure that we at least have a connection. All permissions,
etc., have to be taken care of first."
  (if (or (null *database*) (not (connected-p *database*)))
      (progn
	(connect-toplevel "tanuki1" "tanuki_user" "tanuki_user" "localhost")
	(connected-p *database*))))

(defun tdb-create-tables ()
  (dolist (table *database-tables*)
    (if (not (table-exists-p table))
	(execute (dao-table-definition table)))))

(defun tdb-create-sequences ()
  (dolist (seq *database-sequences*)
    (if (not (sequence-exists-p seq))
	(execute (:create-sequence seq)))))

(defun tdb-clear-database ()
  ; (dolist (table (list-tables))
  (dolist (table (list-tables))
    (execute (:drop-table table)))
  (dolist (seq (list-sequences))
    (execute (:drop-sequence seq))))

;; ;; We could also use some form of postmodern:create-all-tables.
;; ;; TODO: see TODO for tdb-connect.
;; (defun tdb-ready-state (&optional (url-str +default-target+))
;;   (tdb-connect)
;;   (tdb-create-tables)
;;   (tdb-create-sequences))

;; (defun tdb-meta (column-symbol)
;;   (query (:select column-symbol :from 'meta) :single))

(defun tdb-table-count (table-symbol)
  (query (:select (:count '*) :from table-symbol) :single))

(defun tdb-table-dump (table-symbol)
  (query (:select '* :from table-symbol) :plists))

;; Seed the pages with the url from the meta target.
(defun tdb-make-seed-page ()
  (if (= 0 (tdb-table-count 'page))
      (let ((seed-page (make-instance 'page
				      :id (sequence-next 'page-id-seq)
				      :url (query (:select 'target :from 'meta)
						  :single)
				      :internal 1)))
	(insert-dao seed-page)
	(id seed-page))))

;; TODO: see TODO for tdb-connect.
(defun tdb-clean-slate (&optional (url-str +default-target+))
  "Get the system into a usable state from nothing. After running
this, agents should be able to have free reign--the meta table is
populated and the first (target/start) page is defined (with no
hits)."
  (tdb-connect)
  (tdb-clear-database)
  (tdb-create-tables)
  (tdb-create-sequences)
  (let ((new-meta (make-instance 'meta 
				 :start (bb-time:timestamp)
				 :target url-str))
	(first-page (make-instance 'page
				   :id (sequence-next 'page-id-seq)
				   :url url-str
				   :internal 1)))
    (insert-dao new-meta)
    (insert-dao first-page)
    (values (start new-meta)
	    (target new-meta)
	    (id first-page))))

(defun tdb-turn-out-lights ()
  (disconnect-toplevel))

;;;
;;; Targeting.
;;;

(defun random-unvisited-internal-page ()
  "Get a random unvisted page as a DAO, otherwise nil."
  (let ((count (query (:select (:count '*) :from 'page
			       ;; :where (:= 'visited 0))
			       :where (:and (:= 'visited 0)
					    (:= 'internal 1)))
		      :single)))
    (if (not (= 0 count))
	(let ((plist (query (:limit (:select '* :from 'page
					     ;; :where (:= 'visited 0))
					     :where (:and (:= 'visited 0)
							  (:= 'internal 1)))
					     1 (random count))
			    :plist)))
	  (if plist (get-dao 'page (write-to-string (getf plist :id))))))))

(defun update-dao-value (dao slot-name slot-value)
  (setf (slot-value dao slot-name) slot-value)
  (update-dao dao))

;; TODO/BUG: this should be specialized onto a generic html-agent.
;; ((page (get-random-unvisited-page)))
(defun agent-process-page (agent page)
  "..."
  (purge agent)
  (when page
    (update-dao-value page 'visited 1) ; page is now "visited"
    (let ((run-timer (make-instance 'bb-time:timer)))
      (run agent (url page))
      ;; Create the best hit we can at the moment...
      (let ((new-hit (make-instance 'hit
				    :id (sequence-next 'hit-id-seq)
				    :referer (url page)
				    :wait (bb-time:seconds run-timer)
				    :date (bb-time:timestamp))))
	(insert-dao new-hit)
	;; Toggle success and flagged depending on what happened with
	;; the agent.
	(labels ((toggles (hit-dao success-int flagged-int)
			  (update-dao-value hit-dao 'success success-int)
			  (update-dao-value hit-dao 'flagged flagged-int)))
	  (cond
	   ((and (null (errors agent)) (ok-code-p (code agent)))
	    (progn
	      (toggles new-hit 1 0)
	      (dolist (l (links agent))
                (let ((new-page (make-instance 'page
                                               :id (sequence-next 'page-id-seq)
                                               :url l
                                               :internal
                                               (bb-util:bool-to-int
                                                (is-internal-p agent l)))))
                  (insert-dao new-page)))))
	   ((null (errors agent))
	    (progn
	      (toggles new-hit 1 1)
	      (dolist (l (links agent))
                (let ((new-page (make-instance 'page
                                               :id (sequence-next 'page-id-seq)
                                               :url l
                                               :internal
                                               (bb-util:bool-to-int
                                                (is-internal-p agent l)))))
                  (insert-dao new-page)))))
	   (t
	    (progn
	     (toggles new-hit 0 1)))))))))

(defun ok-code-p (code)
  "Decide if a return code is in the OK range or not. Argument may be
a integer or a string. Returns nil"
  (let ((clean-code (if (stringp code)
                        (parse-integer code)
                      code)))
    (if (> (- 400 clean-code) 0) t nil)))

; (select-dao 'page (:= 'visited 0))
; (query-dao 'page (:select '* :from 'page :where (:= 'visited 0)))
; (id (get-dao 'page (write-to-string 1)))

;; ;; We'll just start with random to get things going.
;; (defun next-random-page ()
;;   "Get a random unvisited page from the database."
;;   ;; Check to see that there are unvisted URLs.
;;   (if (= 0 (get-unvisited-page-count +tanuki-db+))
;;       (error 'tanuki-without-targets-error :text "no unvisited pages")
;;     ;; Collect a sample of total pages and a sample of unvisited
;;     ;; pages.
;;     (get-nth-unvisited-page
;;      +tanuki-db+
;;      (random (get-unvisited-page-count +tanuki-db+)))))

;; ;; TODO/BUG: just returning nil makes a hell of a lot more sense.
;; ;; BUG: This is slooooooooooooooooooow after a while.
;; (defun next-target-page ()
;;   "Get the next target page from the system as a page. It will find a
;; page that has not been visited yet."
;;   ;; Check to see that there are unvisted URLs.
;;   (if (= 0 (get-unvisited-page-count +tanuki-db+))
;;       (error 'tanuki-without-targets-error :text "no unvisited pages")
;;     (let ((total-page-count (get-page-count +tanuki-db+))
;; 	  (unvisited-page-count (get-unvisited-page-count +tanuki-db+)))
;;       ;; Collect a sample of total pages and a sample of unvisited
;;       ;; pages.
;;       (let ((total-pages
;; 	     (mapcar #'(lambda (x) (get-nth-page +tanuki-db+ x))
;; 		     (random-sequence +sample-size+
;; 				      :squeeze t
;; 				      :range (list 1 total-page-count))))
;; 	    (unvisited-pages
;; 	     (mapcar #'(lambda (x) (get-nth-unvisited-page +tanuki-db+ x))
;; 		     (random-sequence +sample-size+
;; 				      :squeeze t
;; 				      :range (list 1 unvisited-page-count)))))
;; 	;; TODO/BUG: decide should acually be
;; 	;; working with alists.
;; 	(let ((decision (decide (mapcar #'get-url unvisited-pages)
;; 				(mapcar #'get-url total-pages))))
;; 	  (if decision
;; 	      (get-page-from-url +tanuki-db+ decision)
;; 	    (error 'tanuki-without-targets-error :text "no decision")))))))

;; (defun next-internal-target-page ()
;;   "Get the next internal target page from the system as a page. It
;; will find a page that has not been visited yet."
;;   ;; Check to see that there are unvisted URLs.
;;   (if (= 0 (get-internal-unvisited-page-count +tanuki-db+))
;;       (error 'tanuki-without-targets-error :text "no unvisited internal pages")
;;     (let ((total-page-count (get-internal-page-count +tanuki-db+))
;; 	  (unvisited-page-count (get-internal-unvisited-page-count +tanuki-db+)))
;;       ;; Collect a sample of total pages and a sample of unvisited
;;       ;; pages.
;;       (let ((total-pages
;; 	     (mapcar #'(lambda (x) (get-nth-internal-page +tanuki-db+ x))
;; 		     (random-sequence +sample-size+
;; 				      :squeeze t
;; 				      :range (list 1 total-page-count))))
;; 	    (unvisited-pages
;; 	     (mapcar #'(lambda (x) (get-nth-internal-unvisited-page +tanuki-db+ x))
;; 		     (random-sequence +sample-size+
;; 				      :squeeze t
;; 				      :range (list 1 unvisited-page-count)))))
;; 	;; TODO/BUG: decide should acually be
;; 	;; working with alists.
;; 	(let ((decision (decide (mapcar #'get-url unvisited-pages)
;; 				(mapcar #'get-url total-pages))))
;; 	  (if decision
;; 	      (get-page-from-url +tanuki-db+ decision)
;; 	    (error 'tanuki-without-targets-error :text "no decision")))))))

;; (defun next-mandated-page ()
;;   "Get the next mandated page from the system as a page. It will get
;; them in no particular order."
;;   ;; Check to see that there are unvisted URLs.
;;   (car (sql-engine +tanuki-db+ :mandated t :visited nil)))

;; (defun next-failed-page ()
;;   "Get the next failed page from the system as a page. It will get
;; them in no particular order."
;;   ;; Check to see that there are unvisted URLs.
;;   (car (sql-engine +tanuki-db+ :failed t)))

;; (defun all-failed-pages ()
;;   ""
;;   ;; Check to see that there are unvisted URLs.
;;   ;;(mapcar #'get-url (sql-engine +tanuki-db+ :failed t)))
;;   (sql-engine +tanuki-db+ :failed t))

;; (defun all-odd-pages ()
;;   ""
;;   ;; Check to see that there are unvisted URLs.
;;   ;;(mapcar #'get-url (sql-engine +tanuki-db+ :odd t)))
;;   (sql-engine +tanuki-db+ :odd t))

;; (defun all-mandated-pages ()
;;   ""
;;   ;;(mapcar #'get-url (sql-engine +tanuki-db+ :mandated t)))
;;   (sql-engine +tanuki-db+ :mandated t))

;; (defun mandate-failed ()
;;   "Changes failed pages (that aren't odd) to unvisited mandated pages."
;;   ;; Check to see that there are unvisted URLs.
;;   (let ((pages (sql-engine +tanuki-db+ :failed t :odd nil)))
;;     (loop
;;      for page in pages
;;      do (update-page +tanuki-db+ :page page :mandated t
;; 					    :visited nil
;; 					    :failed nil
;; 					    :odd nil))))

;; ;; TODO: there *has* to be a way to juggle keywords to that I can
;; ;; juggle code with the above function.
;; (defun mandate-odd ()
;;   "Changes odd pages to unvisited mandated pages."
;;   ;; Check to see that there are unvisted URLs.
;;   (let ((pages (sql-engine +tanuki-db+ :odd t)))
;;     (loop
;;      for page in pages
;;      do (update-page +tanuki-db+ :page page :mandated t
;; 					    :visited nil
;; 					    :failed nil
;; 					    :odd nil))))

;; ;; can be used with mandate-failed and mandate-odd
;; (defun do-all-mandates ()
;;   "Retry all failed pages."
;;   (loop
;;    (let ((page (next-mandated-page)))
;;      (if (not page) (return nil)
;;        (progn
;; 	 (format t "Trying mandated:~a~%" (get-url page))
;; 	 (tanuki-step page))))))

;; ;;;
;; ;;; Stepping rules.
;; ;;;

;; ;; 
;; ;(define-condition tanuki-without-targets-warning (warning)
;; ;  ((text :initarg :text :reader text)))
;; (define-condition tanuki-without-targets-error (error)
;;   ((text :initarg :text :reader text)))

;; (defun tanuki-step (current-page)
;;   "A single stepping cycle in the Tanuki system."
;;   ;; Switch on internal or external.
;;   (setf +current-attempt+ (get-url current-page))
;;   (mark-page-as-not-mandated +tanuki-db+ current-page)
;;   (mark-page-as-visited +tanuki-db+ current-page)
;;   (mark-page-with-date +tanuki-db+ current-page)
;;   (let ((timed-time (get-internal-real-time)))
;;     (handler-case
;; 	(if (internal-p (get-url current-page) (get-target +tanuki-db+))
;; 	    (do-internal-step current-page)
;; 	    (do-external-step current-page))
;;       (tanuki-html:page-is-problematic (pip)
;; 	;;(format t "PIP: ~A~%" pip)
;; 	(mark-page-as-failed +tanuki-db+ current-page)
;; 	(mark-page-as-odd +tanuki-db+ current-page)))
;;     ;;
;;     (mark-page-with-time +tanuki-db+ current-page (- (get-internal-real-time)
;; 						     timed-time))))

;; (defun do-internal-step (page)
;;   ""
;;   (format t "internal step ~a~%" (get-url page))
;;   (let ((html-doc (fetch-doc (get-url page))))
;;     (if	(not html-doc)
;; 	(mark-page-as-failed +tanuki-db+ page) ; not a good page
;;       (multiple-value-bind (internal-pages external-pages)
;; 	  (extract-links html-doc (get-target +tanuki-db+))
;; 	(loop
;; 	 for url in internal-pages
;; 	 do (when (not (url-in-db-p +tanuki-db+ url))
;; 	      (enter-page +tanuki-db+ :url url
;; 				      :internal t
;; 				      :referer (get-url page))))
;; 	(loop
;; 	 for url in external-pages
;; 	 do (when (not (url-in-db-p +tanuki-db+ url))
;; 	      (enter-page +tanuki-db+ :url url
;; 				      :internal nil
;; 				      :referer (get-url page))))))))

;; (defun do-external-step (page)
;;   ""
;;   (format t "external step ~a~%" (get-url page))
;;   (if (not (fetch-doc (get-url page)))
;;       (mark-page-as-failed +tanuki-db+ page)))

;; ;;;
;; ;;; Agent control.
;; ;;;

;; (defun start (&optional (page-selector #'next-target-page))
;;   "Start a Tanuki process, if not already going."
;;   (if (not +tanuki-thread+)
;;       (progn
;; 	(format t "Starting (~a)...~%" page-selector)
;; 	(setf +tanuki-thread+
;; ;;	      (sb-thread:make-thread (lambda ()
;; 	      (bordeaux-threads:make-thread (lambda ()
;; 					      (thread-handler page-selector))
;; 					    :name "tanuki thread")))
;;       (format t "Already started.~%")))

;; (defun stop ()
;;   "Politely stop a Tanuki process."
;;   (if +tanuki-thread+
;;       (progn
;; 	(format t "Stopping...~%")
;; 	(setf +stop-signal+ t)
;; 	;;(sb-thread:join-thread +tanuki-thread+)
;; 	(bordeaux-threads:join-thread +tanuki-thread+)
;; 	(setf +tanuki-thread+ nil))
;;     (format t "Already stopped.~%")))

;; (defun thread-handler (page-selector)
;;   "Handle starting and stopping with flags, looping."
;;   (loop
;;      (if +stop-signal+
;; 	 (progn
;; 	   (format t "Handler will stop tanuki...~%")
;; 	   (setf +stop-signal+ nil)
;; 	   (return nil))
;; 	 (progn
;; 	   (format t "Handler will step...~%")
;; 	   (tanuki-step (funcall page-selector))))))

;; ;; TODO: for giggles, see if we can get that into a single format.
;; (defun report-on-failed (&key (long nil))
;;   "Report about all failed pages."
;;   (let ((pages (all-failed-pages)))
;;     (loop
;;      for page in pages
;;      do (progn
;; 	  (format t "FAILED: ~a~%" (get-url page))
;; 	  (when long
;; 	    (format t "ON: ~a~%~%" (get-referer page)))))))

;; (defun report-on-odd (&key (long nil))
;;   "Report about all failed pages."
;;   (let ((pages (all-odd-pages)))
;;     (loop
;;      for page in pages
;;      do (progn
;; 	  (format t "ODD: ~a~%" (get-url page))
;; 	  (when long
;; 	    (format t "ON: ~a~%~%" (get-referer page)))))))

;; ;; BUG: Beware, there is currently a race condition in sqlite3 that
;; ;; may make things go way south if it trips. You might have to
;; ;; disconnect from the lisp and try again. (Suppose we could make our
;; ;; own in the interim.)
;; (defun status ()
;;   "Print the current status of current Tanuki system and return the
;; number of unvisited URLs."
;;   (format t "Current Tanuki status:~%")
;;   (format t "Name: ~a~%" (get-name +tanuki-db+))
;;   (format t "Base URL: ~a~%" (get-target +tanuki-db+))
  
;;   (let ((page-count (get-page-count +tanuki-db+))
;; 	(unvisited-page-count (get-unvisited-page-count +tanuki-db+)))
;;     (format t "Total known pages: ~a~%" page-count)
;;     (format t "Total unvisited pages: ~a~%" unvisited-page-count)
;;     (format t "Total visited pages: ~a~%" (- page-count unvisited-page-count)))
;;   (format t "Visited internal pages: ~a~%"
;; 	  (sql-engine +tanuki-db+ :count t :internal t :visited t))
;;   (format t "Visited external pages: ~a~%"
;; 	  (sql-engine +tanuki-db+ :count t :internal nil :visited t))
;;   ;; (format t "Total interal pages: ~a~%"
;;   ;;	   (sql-engine +tanuki-db+ :count t :internal t))
;;   ;;  (format t "Total exteral pages: ~a~%"
;;   ;;	   (sql-engine +tanuki-db+ :count t :internal nil))
;;   (format t "Total odd pages: ~a~%" (get-odd-page-count +tanuki-db+))
;;   (format t "Total bad pages: ~a~%" (get-bad-page-count +tanuki-db+)))

;; ;; (if *tanuki-thread*
;; ;;    (sb-thread:thread-alive-p *tanuki-thread*)
;; ;;   nil))
;; ;; (format t "(request): ~a~%" +stop-signal+))

;; ;;;
;; ;;; This is a test section for working with forms (somewhat trickier
;; ;;; that trying to just spider links).
;; ;;;

;; ;;;
;; ;;; Functions for parameterizing form inputs.
;; ;;;
