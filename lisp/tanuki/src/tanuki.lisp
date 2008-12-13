;;;;
;;;; Usage: (require 'tanuki)
;;;;        (in-package :tanuki)
;;;;        (set-store)
;;;;        (reconnect-store :id "a_1_6")
;;;;
;;;; WARNING: Threads are not described in CL, so we are, for the time
;;;; being, depending on the SBCL implementation. Shouldn't be hard to
;;;; make it a bit more flexible. Or maybe even a callback system.
;;;;
;;;; Use for comm? http://cl-cookbook.sourceforge.net/sockets.html
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
;;;; BUG:
;;;; (fetch-doc "http://biocyc.org/META/substring-search?type=NIL&object=PARATHION-DEGRADATION-PWY")
;;;;
;;;; BUG:
;;;; (fetch-doc "http://flybase.bio.indiana.edu/reports/FBgn0003471.html")
;;;;
;;;; BUG:
;;;; (FETCH-DOC "http://mips.gsf.de/cgi-bin/proj/funcatDB/search_advanced.pl?action=2&wert=01.05.01.01.05")
;;;;

(defpackage :tanuki
  (:use :cl
	:toolkit
	:tanuki-utils
	:tanuki-html
	:tanuki-decide
	:tanuki-file
	;;:tanuki-web
	:tanuki-db))
(in-package :tanuki)

;;
(defparameter +tanuki-db+ nil
  "Our connection.")
(defparameter +tanuki-store+ '("home" "sjcarbon" "tmp" "tanuki")
  "Location of the store.")
(defparameter +tanuki-data+ "/home/sjcarbon/local/src/svn/geneontology/lisp/tanuki/data"
  "Location of data to be used for forms.")
(defparameter +sample-size+ 33 ; down from 100
  "The size of the random sample in the database for each iteration.")
(defparameter +stop-signal+ nil
  "A flag to indicate that we should end after the current operation.")
(defparameter +tanuki-thread+ nil
  "The keeper of the running tanuki.")
(defparameter +current-attempt+ ""
  "Just for debugging purposes right now.")

;; TODO/BUG: make a current-status object to use for juggling the last
;; reported state without hitting the DB (sqlite3 seems to be having
;; some threading issues (see "http://www.sqlite.org/faq.html#q6")).

;;;
;;; High-level handling
;;;

(defun spin-up ()
  ""
  (set-store)
  (if (probe-store :id "a_1_6")
      (reconnect-store :id "a_1_6")
    (create-store :id "a_1_6"
		  :target
		  "http://localhost/cgi-bin/amigo/go.cgi"))
  (start))

(defun spin-down ()
  (stop))

(defun spin-reset ()
  (hard-reset)
  (hard-reset))

;;;
;;; Store handling.
;;;

(defun set-store ()
  (apply #'warehouse +tanuki-store+))

(defun probe-store (&key (id nil))
  "Check if a store is already extant."
  (set-store)
  (check-warehouse id))

(defun create-store (&key (id nil) (target nil))
  "Set up a Tanuki system in our environment."
  (set-store)
  (setf +tanuki-db+ (make-instance 'tanuki-database))
  (create-tanuki-database +tanuki-db+ :target target
				      :name id
				      :dbfile
				      (namestring (name-to-warehouse id))))

(defun reconnect-store (&key (id nil))
  "Set up a Tanuki system in our environment."
  (set-store)
  (setf +tanuki-db+ (make-instance 'tanuki-database))
  (reconnect-tanuki-database +tanuki-db+ 
			     :dbfile (namestring (name-to-warehouse id))))

(defun close-store ()
  "Set up a Tanuki system in our environment."
  (handler-case
   (progn
     (close-tanuki-database +tanuki-db+))
   (CLSQL-SYS:SQL-FATAL-ERROR (sfe) nil)))

;; TODO: destroy-store

;;;
;;; Targeting.
;;;

;; TODO/BUG: just returning nil makes a hell of a lot more sense.
;; BUG: This is slooooooooooooooooooow after a while.
(defun next-target-page ()
  "Get the next target page from the system as a page. It will find a
page that has not been visited yet."
  ;; Check to see that there are unvisted URLs.
  (if (= 0 (get-unvisited-page-count +tanuki-db+))
      (error 'tanuki-without-targets-error :text "no unvisited pages")
    (let ((total-page-count (get-page-count +tanuki-db+))
	  (unvisited-page-count (get-unvisited-page-count +tanuki-db+)))
      ;; Collect a sample of total pages and a sample of unvisited
      ;; pages.
      (let ((total-pages
	     (mapcar #'(lambda (x) (get-nth-page +tanuki-db+ x))
		     (random-sequence +sample-size+
				      :squeeze t
				      :range (list 1 total-page-count))))
	    (unvisited-pages
	     (mapcar #'(lambda (x) (get-nth-unvisited-page +tanuki-db+ x))
		     (random-sequence +sample-size+
				      :squeeze t
				      :range (list 1 unvisited-page-count)))))
	;; TODO/BUG: decide should acually be
	;; working with alists.
	(let ((decision (decide (mapcar #'get-url unvisited-pages)
				(mapcar #'get-url total-pages))))
	  (if decision
	      (get-page-from-url +tanuki-db+ decision)
	    (error 'tanuki-without-targets-error :text "no decision")))))))

(defun next-internal-target-page ()
  "Get the next internal target page from the system as a page. It
will find a page that has not been visited yet."
  ;; Check to see that there are unvisted URLs.
  (if (= 0 (get-internal-unvisited-page-count +tanuki-db+))
      (error 'tanuki-without-targets-error :text "no unvisited internal pages")
    (let ((total-page-count (get-internal-page-count +tanuki-db+))
	  (unvisited-page-count (get-internal-unvisited-page-count +tanuki-db+)))
      ;; Collect a sample of total pages and a sample of unvisited
      ;; pages.
      (let ((total-pages
	     (mapcar #'(lambda (x) (get-nth-internal-page +tanuki-db+ x))
		     (random-sequence +sample-size+
				      :squeeze t
				      :range (list 1 total-page-count))))
	    (unvisited-pages
	     (mapcar #'(lambda (x) (get-nth-internal-unvisited-page +tanuki-db+ x))
		     (random-sequence +sample-size+
				      :squeeze t
				      :range (list 1 unvisited-page-count)))))
	;; TODO/BUG: decide should acually be
	;; working with alists.
	(let ((decision (decide (mapcar #'get-url unvisited-pages)
				(mapcar #'get-url total-pages))))
	  (if decision
	      (get-page-from-url +tanuki-db+ decision)
	    (error 'tanuki-without-targets-error :text "no decision")))))))

;; TODO/BUG: just returning nil makes a hell of a lot more sense.
(defun next-random-target-page ()
  "Get the next target page from the system as a page. It will find a
page that has not been visited yet."
  ;; Check to see that there are unvisted URLs.
  (if (= 0 (get-unvisited-page-count +tanuki-db+))
      (error 'tanuki-without-targets-error :text "no unvisited pages")
    ;; Collect a sample of total pages and a sample of unvisited
    ;; pages.
    (get-nth-unvisited-page
     +tanuki-db+
     (random (get-unvisited-page-count +tanuki-db+)))))

(defun next-mandated-page ()
  "Get the next mandated page from the system as a page. It will get
them in no particular order."
  ;; Check to see that there are unvisted URLs.
  (car (sql-engine +tanuki-db+ :mandated t :visited nil)))

(defun next-failed-page ()
  "Get the next failed page from the system as a page. It will get
them in no particular order."
  ;; Check to see that there are unvisted URLs.
  (car (sql-engine +tanuki-db+ :failed t)))

(defun all-failed-pages ()
  ""
  ;; Check to see that there are unvisted URLs.
  (sql-engine +tanuki-db+ :failed t))

(defun all-odd-pages ()
  ""
  ;; Check to see that there are unvisted URLs.
  (sql-engine +tanuki-db+ :odd t))

(defun mandate-failed ()
  "Changes failed pages (that aren't odd) to unvisited mandated pages."
  ;; Check to see that there are unvisted URLs.
  (let ((pages (sql-engine +tanuki-db+ :failed t :odd nil)))
    (loop
     for page in pages
     do (update-page +tanuki-db+ :page page :mandated t
					    :visited nil
					    :failed nil
					    :odd nil))))

;; TODO: there *has* to be a way to juggle keywords to that I can
;; juggle code with the above function.
(defun mandate-odd ()
  "Changes odd pages to unvisited mandated pages."
  ;; Check to see that there are unvisted URLs.
  (let ((pages (sql-engine +tanuki-db+ :odd t)))
    (loop
     for page in pages
     do (update-page +tanuki-db+ :page page :mandated t
					    :visited nil
					    :failed nil
					    :odd nil))))

;; can be used with mandate-failed and mandate-odd
(defun do-all-mandates ()
  "Retry all failed pages."
  (loop
   (let ((page (next-mandated-page)))
     (if (not page) (return nil)
       (progn
	 (format t "Trying mandated:~a~%" (get-url page))
	 (tanuki-step page))))))

;;;
;;; Stepping rules.
;;;

;; 
;(define-condition tanuki-without-targets-warning (warning)
;  ((text :initarg :text :reader text)))
(define-condition tanuki-without-targets-error (error)
  ((text :initarg :text :reader text)))

(defun tanuki-step (current-page)
  "A single stepping cycle in the Tanuki system."
  ;; Switch on internal or external.
  (setf +current-attempt+ (get-url current-page))
  (mark-page-as-not-mandated +tanuki-db+ current-page)
  (mark-page-as-visited +tanuki-db+ current-page)
  (mark-page-with-date +tanuki-db+ current-page)
  (let ((timed-time (get-internal-real-time)))
    (handler-case
     (if (internal-p (get-url current-page) (get-target +tanuki-db+))
	 (do-internal-step current-page)
       (do-external-step current-page))
     (tanuki-html:page-is-problematic (pip)
      (mark-page-as-failed +tanuki-db+ current-page)
      (mark-page-as-odd +tanuki-db+ current-page)))
    ;;
    (mark-page-with-time +tanuki-db+ current-page (- (get-internal-real-time)
						     timed-time))))    

(defun do-internal-step (page)
  ""
  (format t "internal step ~a~%" (get-url page))
  (let ((html-doc (fetch-doc (get-url page))))
    (if	(not html-doc)
	(mark-page-as-failed +tanuki-db+ page) ; not a good page
      (multiple-value-bind (internal-pages external-pages)
	  (extract-links html-doc (get-target +tanuki-db+))
	(loop
	 for url in internal-pages
	 do (when (not (url-in-db-p +tanuki-db+ url))
	      (enter-page +tanuki-db+ :url url
				      :internal t
				      :referer (get-url page))))
	(loop
	 for url in external-pages
	 do (when (not (url-in-db-p +tanuki-db+ url))
	      (enter-page +tanuki-db+ :url url
				      :internal nil
				      :referer (get-url page))))))))

(defun do-external-step (page)
  ""
  (format t "external step ~a~%" (get-url page))
  (if (not (fetch-doc (get-url page)))
      (mark-page-as-failed +tanuki-db+ page)))

;;;
;;; Agent control.
;;;

(defun start (&optional (page-selector #'next-target-page))
  "Start a Tanuki process, if not already going."
  (if (not +tanuki-thread+)
      (progn
	(format t "Starting (~a)...~%" page-selector)
	(setf +tanuki-thread+
	      (sb-thread:make-thread (lambda ()
				       (thread-handler page-selector))
				     :name "tanuki thread")))
    (format t "Already started.~%")))

(defun stop ()
  "Politely stop a Tanuki process."
  (if +tanuki-thread+
      (progn
	(format t "Stopping...~%")
	(setf +stop-signal+ t)
	(sb-thread:join-thread +tanuki-thread+)
	(setf +tanuki-thread+ nil))
    (format t "Already stopped.~%")))

;; TODO: make this more severe. And actually work.
(defun hard-reset ()
  "Kill a tanuki process and resets various variables. Last chance to
make things go."
  (close-tanuki-database +tanuki-db+)
  (setf +stop-signal+ nil)
  (setf +tanuki-thread+ nil)
  (format t "Hopefully killed.~%"))

(defun thread-handler (page-selector)
  "Handle starting and stopping with flags, looping."
  (loop
     (if +stop-signal+
	 (progn
	   (format t "Handler will stop tanuki...~%")
	   (setf +stop-signal+ nil)
	   (return nil))
	 (progn
	   (format t "Handler will step...~%")
	   (tanuki-step (funcall page-selector))))))

;; TODO: for giggles, see if we can get that into a single format.
(defun report-on-failed (&key (long nil))
  "Report about all failed pages."
  (let ((pages (all-failed-pages)))
    (loop
     for page in pages
     do (progn
	  (format t "FAILED: ~a~%" (get-url page))
	  (when long
	    (format t "ON: ~a~%~%" (get-referer page)))))))

(defun report-on-odd (&key (long nil))
  "Report about all failed pages."
  (let ((pages (all-odd-pages)))
    (loop
     for page in pages
     do (progn
	  (format t "ODD: ~a~%" (get-url page))
	  (when long
	    (format t "ON: ~a~%~%" (get-referer page)))))))

;; BUG: Beware, there is currently a race condition in sqlite3 that
;; may make things go way south if it trips. You might have to
;; disconnect from the lisp and try again. (Suppose we could make our
;; own in the interim.)
(defun status ()
  "Print the current status of current Tanuki system and return the
number of unvisited URLs."
  (format t "Current Tanuki status:~%")
  (format t "Name: ~a~%" (get-name +tanuki-db+))
  (format t "Base URL: ~a~%" (get-target +tanuki-db+))
  
  (let ((page-count (get-page-count +tanuki-db+))
	(unvisited-page-count (get-unvisited-page-count +tanuki-db+)))
    (format t "Total known pages: ~a~%" page-count)
    (format t "Total unvisited pages: ~a~%" unvisited-page-count)
    (format t "Total visited pages: ~a~%" (- page-count unvisited-page-count)))
  (format t "Visited internal pages: ~a~%"
	  (sql-engine +tanuki-db+ :count t :internal t :visited t))
  (format t "Visited external pages: ~a~%"
	  (sql-engine +tanuki-db+ :count t :internal nil :visited t))
  ;; (format t "Total interal pages: ~a~%"
  ;;	   (sql-engine +tanuki-db+ :count t :internal t))
  ;;  (format t "Total exteral pages: ~a~%"
  ;;	   (sql-engine +tanuki-db+ :count t :internal nil))
  (format t "Total odd pages: ~a~%" (get-odd-page-count +tanuki-db+))
  (format t "Total bad pages: ~a~%" (get-bad-page-count +tanuki-db+)))

;; (if *tanuki-thread*
;;    (sb-thread:thread-alive-p *tanuki-thread*)
;;   nil))
;; (format t "(request): ~a~%" +stop-signal+))

;;;
;;; This is a test section for working with forms (somewhat trickier
;;; that trying to just spider links).
;;;

;;;
;;; Functions for parameterizing form inputs.
;;;

;;(defun 