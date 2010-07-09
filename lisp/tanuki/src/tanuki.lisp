;;;; -*- mode: Lisp -*-
;;;;
;;;; psql -W -U tanuki_user -h localhost tanuki
;;;;
;;;; From the beginning:
;;;;    (require 'tanuki)
;;;;    (in-package :tanuki)
;;;;    (top-reset "http://localhost/cgi-bin/amigo/amigo" "/data")
;;;;    (start-a-tanuki)
;;;;    (start-a-tanuki)
;;;;    [tail -f /tmp/tanuki.log]
;;;;    (flag-down)
;;;;
;;;; Continue:
;;;;    (top-remember)
;;;;    (start-a-tanuki)
;;;;    (start-a-tanuki)
;;;;    [tail -f /tmp/tanuki.log]
;;;;    (flag-down)
;;;;
;;;; Partial:
;;;;    (flag-up)
;;;;    (setf a (make-instance 'tanuki-agent :home-url *default-url*))
;;;;    (process-argument-set a (random-undone-argument-set 'internal))
;;;;    (flag-down)
;;;;
;;;; WISHLIST:
;;;; *) User agent string.
;;;; *) Be able to report about running threads.
;;;; *) Be able to problems-to-mandates and run them. Is keeping the
;;;;    history a good idea?
;;;; *) Sampling (like we used to) for better results/coverage.
;;;; *) Add "action" table to keep track of tanuki life events.
;;;; *) "Class"ify tanuki so we can work our way towards a web-based
;;;;     general interface for multiple sites.
;;;; *) Form detection, profiling, and running.
;;;; *) Fuzzing and input (link and form) generation.
;;;; *) Web interface.
;;;; *) Toy frontend extension language with safe function calls as
;;;;    API.
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
	;;:tanuki-decide
	)
  ;; (:import-from :tanuki-schema
  ;;               :url
  ;;               :page-id
  ;;               :page
  ;;               :todo
  ;;               :internal)
  )
(in-package :tanuki)


(defvar *default-url* "http://localhost/"
  "The default target used when not explicitly defined.")
(defvar *default-data* "/tmp"
  "The default data location used when not explicitly defined.")

(defparameter +default-logger+
  (make-instance 'bb-log:simple-log :log-out "/tmp/tanuki.log")
  "The default logger.")
;; (defparameter +sample-size+ 33 ; down from 100
;;   "The size of the random sample in the database for each iteration.")


(defun kvetch (obj)
  "Wrapper."
  (bb-log:kvetch +default-logger+ obj))

(defun top-reset (new-default-url new-default-data)
  "Blows away everything for a fresh start with the given url."
  (flag-up)
  (setf *default-url* new-default-url)
  (setf *default-data* new-default-data)
  (tanuki-db:reset-database *default-url* *default-data*)
  t)

(defun top-remember ()
  "Try to get ready using the database."
  (flag-up)
  (tanuki-db:connect-repl)
  (setf *default-url* (query (:select 'target :from 'meta) :single))
  (setf *default-data* (query (:select 'data :from 'meta) :single))
  t)

;;;
;;; Operations: changing values in the database.
;;;

(defun update-dao-value (dao slot-name slot-value)
  (setf (slot-value dao slot-name) slot-value)
  (update-dao dao))

(defun argument-set-to-page (argument-set)
  "Convert an into an easily consumable plist form."
  ;; Join arg to arg-set to page
  (let ((page-id (tanuki-schema:page-id argument-set)))
    (get-dao 'tanuki-schema:page page-id)))

;; TODO/BUG: Proper (table) arguments too.
(defun process-agent-links (agent)
  (dolist (alink (links agent))
    (with-slots
     (base-url raw-url clean-url) alink
     (with-transaction ()
      (let ((curr-page-id (page-id-by-url base-url)))
        ;; Ensure that we have a page and id to work with.
        (when (not curr-page-id)
          (let ((new-page (make-instance 'tanuki-schema:page
                                         :id (sequence-next 'page-id-seq)
                                         :url base-url
                                         :internal
                                         (bb-util:bool-to-int
                                          (is-internal-p agent base-url)))))
            (insert-dao new-page)
            (setf curr-page-id (tanuki-schema:id new-page))))
        ;; Create a new argument set.
        (when (not (argument-set-id-by-url clean-url))
          (let ((new-aset (make-instance 'tanuki-schema:argument-set
                                         :id (sequence-next
                                              'argument-set-id-seq)
                                         :page-id curr-page-id
                                         :raw-url raw-url
                                         :clean-url clean-url
                                         :reference (current-url agent)
                                         :request-method "GET"
                                         :request-type "link"
                                         :todo 1)))
            (insert-dao new-aset)
            ;; Add arguments to argument table.
            (let ((new-aset-id (tanuki-schema:id new-aset)))
              (dolist (pair (query-list alink))
                (let ((new-arg (make-instance 'tanuki-schema:argument
                                              :id (sequence-next
                                                   'argument-id-seq)
                                              :argument-set-id new-aset-id
                                              :name (car pair)
                                              :value (cdr pair))))
                  (insert-dao new-arg))))
            )))))))

;;;
;;; Targeting finding a page to try in the database.
;;;

;; select * from argument_set inner join page on (argument_set.page_id = page.id) where argument_set.todo = 1 and page.internal = 1;
(defun random-undone-argument-set (loc-symbol)
  "Get a random unvisted page as a DAO, otherwise nil. Takes the
symbols 'internal, 'external, 'weighed, or 'random."
  (let* ((in-ex (cond
                 ((eq loc-symbol 'external) 0)
                 ((eq loc-symbol 'internal) 1)
                 ((eq loc-symbol 'weighed)
                  (alexandria:random-elt '(0 1 1 1 1)))
                 (t (alexandria:random-elt '(0 1)))))
         (count (query (:select (:count '*)
                                :from 'argument-set
                                :inner-join 'page
                                :on (:= 'argument-set.page_id 'page.id)
                                :where (:and (:= 'argument-set.todo 1)
                                             (:= 'page.internal in-ex)))
                       :single)))
    (if (not (= 0 count))
	(let ((plist
               (query (:limit (:select '*
                                       :from 'argument-set
                                       :inner-join 'page
                                       :on (:= 'argument-set.page_id 'page.id)
                                       :where (:and (:= 'argument-set.todo 1)
                                                    (:= 'page.internal in-ex)))
                              1 (random count))
                      :plist)))
	  (if plist (get-dao 'tanuki-schema:argument-set (getf plist :id)))))))

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


;;
(defun process-argument-set (agent aset)
  "..."
  (kvetch (format nil "~a looking at: ~a" agent
                  (tanuki-schema:clean-url aset)))
  (when aset
    (update-dao-value aset 'tanuki-schema:todo 0) ; set is now off todo list
    (let ((run-timer (make-instance 'bb-time:timer)))
      (fetch agent (tanuki-schema:clean-url aset))
      ;; Create the best hit we can at the moment...
      (let* ((ret-code (if (null (code agent)) :null (code agent)))
             (new-hit (make-instance 'tanuki-schema:hit
                                     :id (sequence-next 'hit-id-seq)
                                     :argument-set-id (tanuki-schema:id aset)
                                     :wait (bb-time:seconds run-timer)
                                     :date (bb-time:timestamp)
                                     ;; :agent # TODO
                                     :code ret-code)))
	(insert-dao new-hit)
	;; Toggle success and flagged depending on what happened with
	;; the agent.
	(labels ((toggles (hit-dao success-int flagged-int)
			  (update-dao-value hit-dao
                                            'tanuki-schema:success
                                            success-int)
			  (update-dao-value hit-dao
                                            'tanuki-schema:flagged
                                            flagged-int)))
          ;; 
	  (cond
	   ((and (null (errors agent)) (is-code-ok-p agent))
            (toggles new-hit 1 0))
	   ((null (errors agent))
            (toggles new-hit 1 1))
	   (t (toggles new-hit 0 1)))))
      ;; Only add links (spider forward) when it is an internal link.
      (when (is-internal-p agent (current-url agent))
        (process-agent-links agent)))))

;; 
(defun tanuki-step (agent)
  "Run an agent until it shouldn't."
  (kvetch (format nil "Starting agent ~a..." agent))
  (do ()
      ((not (flag-p)))
    ;; TODO: What is best here?
    ;; (let ((page (random-undone-argument-set 'internal)))
    (let ((aset (random-undone-argument-set 'weighed)))
      (if aset
          (process-argument-set agent aset)
        (progn
          (kvetch (format nil "~a is waiting for something new..." agent))
          (sleep 60)))))
  (kvetch (format nil "Agent ~a is stopping" agent)))

;; TODO: see the old runner for target selector stuff...
;; BUG (when running multiple tanukis):
;;    Database error: This connection is still processing another query.
;;       [Condition of type CL-POSTGRES:DATABASE-ERROR]
;; They must need their own connection...
(defun start-a-tanuki ()
  (let ((agent (make-instance 'tanuki-agent :home-url *default-url*)))
    (bordeaux-threads:make-thread
     (lambda ()
       (with-connection
        tanuki-db:*connection-parameters*
        (tanuki-step agent)))
     :name (symbol-name (gensym)))))

;;;
;;; Report queries from connected toplevel.
;;;

(defmacro row-count (table-symbol &optional (where-list nil))
  "Simplify getting usable simple row counts from tables. Int or nil."
  (let ((base-query `(:select (:count '*) :from ,table-symbol)))
    (when where-list
      (setf base-query (append base-query (list :where) (list where-list))))
    `(query ,base-query :single)))

(defun get-plist-by-id (table-symbol id)
  "Get a table's row's plist by id."
  (query (:select '* :from table-symbol :where (:= 'id id)) :plist))
  
(defun page-id-by-url (page-url)
  "Int or nil on the existance of a page (by url string) in the database."
  (query (:select 'id
                  :from 'page
                  :where (:= 'url page-url)) :single))

(defun argument-set-id-by-url (url &optional (type 'clean))
  "Int or nil on the existance of an arg set (by url string) in the database."
  (let ((field (if (eq type 'clean) 'clean-url 'raw-url)))
    (query (:select 'id
                    :from 'argument-set
                    :where (:= field url)) :single)))

(defun general-report ()
  "Print the current status of current Tanuki system and return the
number of unvisited URLs."
  (let ((target (query (:select 'target :from 'meta) :single))
        (start (bb-time:date-string
                (query (:select 'start :from 'meta) :single)))
        (external-page-count (row-count 'page (:= 'internal 0)))
        (internal-page-count (row-count 'page (:= 'internal 1)))
        (aset-count (row-count 'argument-set))
        (hit-count (row-count 'hit))
        (flagged-count (row-count 'hit (:= 1 'flagged)))
        (error-count (row-count 'hit (:= 0 'success))))
    (format t "Current Tanuki status:~%")
    (format t "Base URL: ~a~%" target)
    (format t "Started at: ~a~%" start)
    (format t "Known external pages: ~a~%" external-page-count)
    (format t "Known internal pages: ~a~%" internal-page-count)
    (format t "Collected argument sets: ~a~%" aset-count)
    (format t "Attempted page visits: ~a~%" hit-count)
    (format t "Total flagged pages: ~a~%" flagged-count)
    (format t "Total errors: ~a~%" error-count)))

(defun error-report ()
  "Give a report about all of the \"error\" (non-success) pages."
  (let ((qlist (query (:select '* :from  'hit
                               :inner-join 'argument-set
                               :on (:= 'hit.argument_set_id 'argument-set.id)
                               :where (:= 'hit.success 0)) :plists))
        (count 0))
    (dolist (q qlist)
      (setf count (+ 1 count))
      (format t  "~%")
      (format t  "Page ID: ~a~%" (getf q :page-id))
      (format t  "Date: ~a~%" (bb-time:humanstamp (getf q :date)))
      (format t  "Processing time: ~a~%" (getf q :wait))
      (format t  "Code: ~a~%" (getf q :code))
      (format t  "Flagged: ~a~%" (getf q :flagged))
      (format t  "Reference: ~a~%" (getf q :reference))
      (format t  "URL: ~a~%" (getf q :clean-url)))
    count))
 
(defun flagged-report ()
  "Give a report about all of the \"flagged\" pages."
  (let ((qlist (query (:select '* :from  'hit
                               :inner-join 'argument-set
                               :on (:= 'hit.argument_set_id 'argument-set.id)
                               :where (:= 'hit.flagged 1)) :plists))
        (count 0))
    (dolist (q qlist)
      (setf count (+ 1 count))
      (format t  "~%")
      (format t  "Page ID: ~a~%" (getf q :page-id))
      (format t  "Date: ~a~%" (bb-time:humanstamp (getf q :date)))
      (format t  "Processing time: ~a~%" (getf q :wait))
      (format t  "Code: ~a~%" (getf q :code))
      (format t  "Success: ~a~%" (getf q :success))
      (format t  "Reference: ~a~%" (getf q :reference))
      (format t  "URL: ~a~%" (getf q :clean-url)))
    count))

; (select-dao 'page (:= 'todo 1))
; (query-dao 'page (:select '* :from 'page :where (:= 'todo 1)))
; (id (get-dao 'page (write-to-string 1)))

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
