;;;; -*- mode: Lisp -*-
;;;;
;;;; TODO: Add and use "arguments" table keyed to page--fixes future
;;;; problems now. Add link arguments.
;;;;
;;;; TODO: Less thinking, more alexandria.
;;;;
;;;; BUG: CLSQL (and so our sqlite3 store) seems to be hosed right
;;;; now, possibly due to uffi something or other, so we need to
;;;; switch. Even though I really liked cl-prevalence (memory +
;;;; serialization), easy relational functionality is necessary so
;;;; I'll go with postmodern.
;;;;
;;;; psql -W -U tanuki_user -h localhost tanuki
;;;;
;;;; From the beginning:
;;;;    (require 'tanuki)
;;;;    (in-package :tanuki)
;;;;    (top-reset "http://localhost/cgi-bin/amigo/amigo")
;;;;    (start-a-tanuki)
;;;;    (start-a-tanuki)
;;;;    (flag-down)
;;;;
;;;; Continue:
;;;;    (top-remember)
;;;;    (start-a-tanuki)
;;;;    (start-a-tanuki)
;;;;    (flag-down)
;;;;
;;;; Partial:
;;;;    (flag-up)
;;;;    (setf a (make-instance 'tanuki-agent :base-url *default-url*))
;;;;    (process-page a (random-untried-internal-page))
;;;;    (flag-down)
;;;;
;;;; TODO: Make the fail/odd system also report a "comment" about the
;;;; problem type.
;;;;
;;;; TODO: multiple target selection--internal, external, etc.
;;;;
;;;; TODO: be able to convert fails to mandates.
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
	:postmodern
	:tanuki-agent
	:tanuki-db
	;;:tanuki-decide
	))
(in-package :tanuki)

;; ;;
;; (defparameter +tanuki-data+ "/home/sjcarbon/local/src/svn/geneontology/lisp/tanuki/data"
;;   "Location of data to be used for forms.")
;; (defparameter +sample-size+ 33 ; down from 100
;;   "The size of the random sample in the database for each iteration.")
;; (defparameter +tanuki-thread+ nil
;;   "The keeper of the running tanuki.")

(defvar *default-url* "http://localhost/"
  "The default target used when not explicitly defined.")

(defvar +default-logger+
  (make-instance 'bb-log:simple-log :log-out "/tmp/tanuki.log")
  "The default logger.")

(defun kvetch (obj)
  "Wrapper."
  (bb-log:kvetch +default-logger+ obj))

(defun top-reset (new-default-url)
  "Blows away everything for a fresh start with the given url."
  (flag-up)
  (setf *default-url* new-default-url)
  (reset-database *default-url*))

;;;
;;; Queries: finding things out about the database.
;;;

(defun top-remember ()
  "Try to get ready using the database."
  (flag-up)
  (connect-repl)
  (setf *default-url* (query (:select 'target :from 'meta) :single)))

(defun page-extant-p (page-url)
  "t or nil on the existance of a page (by url string) in the database."
  (let ((count (query (:select (:count '*) :from 'page
                               :where (:= 'url page-url))
		      :single)))
    (if (= count 0) nil t)))

;;;
;;; Operations: changing values in the database.
;;;

(defun update-dao-value (dao slot-name slot-value)
  (setf (slot-value dao slot-name) slot-value)
  (update-dao dao))

;; TODO/BUG: need to separate arguments and pages.
(defun insert-agent-links (agent)
  (dolist (l (links agent))
    ;; TODO: This should not be "when", but "if" depending on whether
    ;; we do page and args or just page.
    (with-transaction ()
      (when (not (page-extant-p l))
        (let ((new-page (make-instance 'page
                                       :id (sequence-next 'page-id-seq)
                                       :url l
                                       :internal
                                       (bb-util:bool-to-int
                                        (is-internal-p agent l)))))
        (insert-dao new-page))))))

;;;
;;; Targeting finding a page to try in the database.
;;;

(defun random-untried-internal-page ()
  "Get a random unvisted page as a DAO, otherwise nil."
  (let ((count (query (:select (:count '*) :from 'page
			       ;; :where (:= 'tried 0))
			       :where (:and (:= 'tried 0)
					    (:= 'internal 1)))
		      :single)))
    (if (not (= 0 count))
	(let ((plist (query (:limit (:select '* :from 'page
					     ;; :where (:= 'tried 0))
					     :where (:and (:= 'tried 0)
							  (:= 'internal 1)))
					     1 (random count))
			    :plist)))
	  (if plist (get-dao 'page (write-to-string (getf plist :id))))))))

;;;
;;; Agent control: 
;;;

(defparameter +loop-flag+ t
  "Let working agents know they can stop.")
(defun flag-p ()
  +loop-flag+)
(defun flag-up ()
  (setf +loop-flag+ t))
(defun flag-down ()
  (setf +loop-flag+ nil))


;; TODO/BUG: this should be specialized onto a generic html-agent.
;; ((page (get-random-untried-page)))
(defun process-page (agent page)
  "..."
  (when page
    (update-dao-value page 'tried 1) ; page is now "tried"
    (let ((run-timer (make-instance 'bb-time:timer)))
      (fetch agent (url page))
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
              (insert-agent-links agent)))
	   ((null (errors agent))
	    (progn
	      (toggles new-hit 1 1)
              (insert-agent-links agent)))
	   (t
	    (progn
	     (toggles new-hit 0 1)))))))))

;(defun 

;; 
(defun tanuki-loop ()
  "Run an agent until it shouldn't."
  (let ((agent (make-instance 'tanuki-agent :base-url *default-url*)))
    (kvetch (format nil "Starting agent ~a..." agent))
    (do ()
        ((not (flag-p)))
      (let ((page (random-untried-internal-page)))
        (if page
            (progn
              (kvetch (format nil "~a looking at: ~a" agent (url page)))
              (process-page agent page))
          (progn
            (kvetch (format nil "~a is waiting a minute..." agent))
            (sleep 60)))))))

;; TODO: see the old runner for target selector stuff...
;; BUG (when running multiple tanukis):
;;    Database error: This connection is still processing another query.
;;       [Condition of type CL-POSTGRES:DATABASE-ERROR]
;; They must need their own connection...
(defun start-a-tanuki ()
  (bordeaux-threads:make-thread
   (lambda ()
     (with-connection
      tanuki-db::*connection-parameters*
      (tanuki-loop)))
  :name (symbol-name (gensym))))



;;;
;;; Elsewhere: where these code bits should be.
;;;

(defun ok-code-p (code)
  "Decide if a return code is in the OK range or not. Argument may be
a integer or a string. Returns nil"
  (let ((clean-code (if (stringp code)
                        (parse-integer code)
                      code)))
    (if (> (- 400 clean-code) 0) t nil)))

; (select-dao 'page (:= 'tried 0))
; (query-dao 'page (:select '* :from 'page :where (:= 'tried 0)))
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
