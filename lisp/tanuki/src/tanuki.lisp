;;;;
;;;; BUG: CLSQL seems to be hosed right now, possibly due to uffi
;;;; something or other; to use this, we need to switch--currently
;;;; evaluating postmodern (postgres) and cl-prevalence (memory +
;;;; serialization)... A little more testing, and it looks like
;;;; cl-prevalence is insanely easy to use (a least in the small cases
;;;; I looked at)--try this first, it may be easiest to get something
;;;; rolling and, since it is all lisp, there will be no uffi
;;;; problems. Of course, it will depend on how well trivial sql-like
;;;; search go over the data...That last part turns out to be a
;;;; problem with the way I want to structure the data; now I'll look
;;;; at postmodern...
;;;;
;;;; Usage: (require 'tanuki)
;;;;        (in-package :tanuki)
;;;;        (set-store)
;;;;        (reconnect-store :id "a_1_8")
;;;;        ;; (spin-up)
;;;;
;;;; WARNING: Using bordeaux threads.
;;;;
;;;; Use for comm? http://cl-cookbook.sourceforge.net/sockets.html
;;;;
;;;; README/NOTE: One thing that would get rid of quite a few of the
;;;; problems in here would be to switch off of the sqlite3 backend
;;;; (cause of many many headaches) into something more lispy, like
;;;; elephant (actually, it looks dead), cl-prevalence, cl-perec,
;;;; postmodern (most likely right now), submarine, ...
;;;;
;;;; TODO: Currently, only one tanuki can run at a time in an
;;;; environment--this should be fixed.
;;;;
;;;; TODO: add response time to URL
;;;;
;;;; TODO: Make the fail/odd system also report a "comment" about the
;;;; problem type.
;;;;
;;;; TODO: little wrapper for 'ab', the apache benchmarking tool.
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
	:tanuki-agent
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

;; TODO/BUG: make a current-status object to use for juggling the last
;; reported state without hitting the DB (sqlite3 seems to be having
;; some threading issues (see "http://www.sqlite.org/faq.html#q6")).

;; ;;;
;; ;;; Start of cl-prevalence high-level.
;; ;;;

;; (defparameter +tanuki-system-location+ "/home/sjcarbon/tmp/tanuki/foo1"
;;   "Location of the store.")

;; (defclass tanuki-db ()
;;   ((links
;;     :accessor get-links
;;     :initform (make-hash-table :test 'equal))
;;    (target
;;     :accessor get-target
;;     :initarg :target)))
   
;; (defclass link ()
;;   ((id
;;     :accessor get-id
;;     :initform nil)
;;    (url
;;     :accessor get-url
;;     :initarg :url)
;;    (hits
;;     :accessor get-hits
;;     :initform (make-hash-table :test 'equal))
;;    (mandated
;;     :accessor mandateed-p
;;     :initform nil)
;;    (internal
;;     :accessor internal-p
;;     :initform nil)))

;; (defclass hit ()
;;   ((id
;;     :accessor get-id
;;     :initform nil)
;;    (referer
;;     :accessor get-referer
;;     :initform nil)
;;    (time
;;     :accessor get-time
;;     :initform nil)
;;    (date
;;     :accessor get-date
;;     :initform nil)
;;    (flagged
;;     :accessor flagged-p
;;     :initform nil)
;;    (success
;;     :accessor success-p
;;     :initform nil)))
    
;; ;; (defun prep ()
;; ;;   "Preparation and stuff."
;; ;;   (gs-fs:make-directory +tanuki-store+)
  
;;;
;;; High-level database handling.
;;;

;; (dao-table-definition 'meta) looks correct...
(defclass meta ()
  ((start
    :accessor get-start
    :col-type bigint
    :initarg :start)
   (target
    :accessor get-target
    :col-type string
    :initarg :target))
  (:metaclass dao-class))
   
(defclass page ()
  ((id
    :accessor get-id
    :col-type bigint
    :initarg :id)
   (url
    :accessor get-url
    :col-type string
    :initarg :url)
   (internal
    :accessor internal-p
    :col-type bigint ; TODO: internal needs to be decided quickly
    :initarg :internal)
   (mandated
    :accessor mandated-p
    :col-type bigint
    :col-default 0)
   (visited
    :accessor visited-p
    :col-type bigint
    :col-default 0))
  (:metaclass dao-class)
  (:keys id))

;; (dao-table-definition 'hit) looks correct...
(defclass hit ()
  ((id
    :accessor get-id
    :col-type bigint)
   (arguments
    :accessor get-arguments
    :col-type (or db-null string)
    :initform nil)
   (referer
    :accessor get-referer
    :col-type (or db-null string)
    :initform nil)
   (time
    :accessor get-time
    :col-type (or db-null bigint) ; TODO: time init.
    :initform nil)
   (date
    :accessor get-date
    :col-type (or db-null bigint) ; TODO: date init.
    :initform nil)
   (flagged
    :accessor flagged-p
    :col-type (or db-null bigint)
    :initform nil)
   (success
    :accessor success-p
    :col-type (or db-null bigint)
    :initform nil))
  (:metaclass dao-class)
  (:keys id))

(defvar *database-tables* '(meta page hit)
  "All the tables that are used in Tanuki's database.")
(defvar *database-sequences* '(page-id-seq hit-id-seq)
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

;; Example usage: (tdb-time-stamp (get-universal-time))
;; Example usage: (tdb-time-stamp)
(defun tdb-time-stamp (&optional (utime (get-universal-time)))
  "Return a date integer representing the inputted utime."
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time utime)
    (declare (ignore tz dst-p day-of-week))
    ;; Sorry for let--don't want to return two values here.
    (let ((retval (parse-integer (format nil "~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
					 year
					 month
					 date
					 hour
					 minute
					 second))))
      retval)))

;; ;; We could also use some form of postmodern:create-all-tables.
;; ;; TODO: see TODO for tdb-connect.
;; (defun tdb-ready-state (&optional (url-str +default-target+))
;;   (tdb-connect)
;;   (tdb-create-tables)
;;   (tdb-create-sequences))

(defun tdb-meta (column-symbol)
  (query (:select column-symbol :from 'meta) :single))

(defun tdb-table-count (table-symbol)
  (query (:select (:count '*) :from table-symbol) :single))

;; Seed the pages with the url from the meta target.
(defun tdb-make-seed-page ()
  (if (= 0 (tdb-table-count 'page))
      (let ((seed-page (make-instance 'page
				      :id (sequence-next 'page-id-seq)
				      :url (tdb-meta 'target)
				      :internal 1)))
	(insert-dao seed-page)
	(get-id seed-page))))

;; TODO: see TODO for tdb-connect.
(defun tdb-clean-slate (&optional (url-str +default-target+))
  (tdb-connect)
  (tdb-clear-database)
  (tdb-create-tables)
  (tdb-create-sequences)
  (let ((new-meta (make-instance 'meta 
				 :start (tdb-time-stamp)
				 :target url-str)))
    (insert-dao new-meta)
    (values (get-start new-meta)
	    (get-target new-meta))))

(defun tdb-turn-out-lights ()
  (disconnect-toplevel))

;(query (:select 'start :from 'meta) :single)
;; INSERT INTO foo (ai, bar) VALUES (nextval('ai_seq'), 99);
;; (sequence-next 'hit-id-seq) => INT

;;;
;;; Targeting.
;;;

;(query "select count(*) from meta" :single)
;(query (:select (:count '*) :from 'meta) :single)

;; (defprepared sovereign-of
;;   (:select 'sovereign :from 'country :where (:= 'name '$1))
;;   :single!)
;; (sovereign-of "The Netherlands")
;; ;; => "Beatrix"

(let ((foo 0))
  (defun get-foo ()
    (incf foo)))

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

;; ;; TODO/BUG: just returning nil makes a hell of a lot more sense.
;; (defun next-random-target-page ()
;;   "Get the next target page from the system as a page. It will find a
;; page that has not been visited yet."
;;   ;; Check to see that there are unvisted URLs.
;;   (if (= 0 (get-unvisited-page-count +tanuki-db+))
;;       (error 'tanuki-without-targets-error :text "no unvisited pages")
;;     ;; Collect a sample of total pages and a sample of unvisited
;;     ;; pages.
;;     (get-nth-unvisited-page
;;      +tanuki-db+
;;      (random (get-unvisited-page-count +tanuki-db+)))))

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
