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
;;;;    (stop-tanukis)
;;;;
;;;; Continue:
;;;;    (top-remember)
;;;;    (start-a-tanuki)
;;;;    (start-a-tanuki)
;;;;    [tail -f /tmp/tanuki.log]
;;;;    (stop-tanukis)
;;;;
;;;; Partial:
;;;;    (flag-up)
;;;;    (setf a (make-agent))
;;;;    (process-argument-set a (random-undone-argument-set))
;;;;    (flag-down)
;;;;
;;;; WISHLIST:
;;;; *) Be able to report about running threads.
;;;; *) Sampling (like we used to) for better results/coverage.
;;;; *) Add "action" table to keep track of tanuki life events.
;;;; *) "Class"ify tanuki so we can work our way towards a web-based
;;;;     general interface for multiple sites running in parallel.
;;;; *) Low-frequency agents for background checking.
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
	:tanuki-orm
	:tanuki-decide)
  (:import-from :tanuki-db
                :set-connection)
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

(defun debug-sql (sym)
  ""
  (if (eq sym 'on)
      (setf cl-postgres:*query-log* *standard-output*)
    (setf cl-postgres:*query-log* nil)))

(defun debug-http (sym)
  ""
  (if (eq sym 'on)
      (setq drakma:*header-stream* *standard-output*)
    (setq drakma:*header-stream* nil)))

(defun kvetch (obj)
  "Wrapper; with stdout (for interactive operations) too."
  (format nil "~a~%" obj)
  (bb-log:kvetch +default-logger+ obj))

(defun make-agent ()
  "Quick agent maker."
  (make-instance 'tanuki-agent :home-url *default-url*))

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
                                     :agent (user-agent agent)
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
	   (t (toggles new-hit 0 1))))
        ;; Add errors/comments to the database if any.
        (dolist (e (errors agent))
          ;;(break "3" (tanuki-schema:id new-hit))
          (let ((new-comment (make-instance 'tanuki-schema:comment
                                            :id (sequence-next
                                                 'comment-id-seq)
                                            :hit-id (tanuki-schema:id new-hit)
                                            :comment-type "error"
                                            :text e)))
            (insert-dao new-comment))))
      ;; Only add links (spider forward) when it is an internal link.
      (when (is-internal-p agent (current-url agent))
        (process-agent agent)))))

(defun process-agent (agent)
  "..."
  (dolist (alink (links agent))
    (with-slots
     (base-url raw-url clean-url) alink
     (with-transaction ()
      ;; Ensure that we have a page and id to work with.
      (let* ((curr-page (parse-page base-url))
             (curr-page-id (when curr-page (tanuki-schema:id curr-page))))
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
        (when (not (parse-argument-set clean-url))
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

;; Option to control the type of target that we're looking at.
(defparameter +random-target-type+ 'weighed
  ;; 'internal 'external 'weighed 'random
  "Choose the type of target to select when looking for a new argument
  set. Can be: 'internal, 'external, 'weighed, or 'random.")

;; select * from argument_set inner join page on (argument_set.page_id = page.id) where argument_set.todo = 1 and page.internal = 1;
(defun random-undone-argument-set ()
  "Get a random unvisted page as a DAO, otherwise nil. Takes the
symbols: 'internal, 'external, 'weighed, or 'random."
  (let* ((rttype +random-target-type+)
         (in-ex (cond
                 ((eq rttype 'external) 0)
                 ((eq rttype 'internal) 1)
                 ((eq rttype 'weighed)
                  (alexandria:random-elt '(0 1 1 1 1)))
                 (t (alexandria:random-elt '(0 1)))))
         (count (query (:select (:count '*)
                                :from 'argument-set
                                :inner-join 'page
                                :on (:= 'argument-set.page-id 'page.id)
                                :where (:and (:= 'argument-set.todo 1)
                                             (:= 'page.internal in-ex)))
                       :single)))
    (if (not (= 0 count))
	(let ((plist
               (query (:limit (:select '*
                                       :from 'argument-set
                                       :inner-join 'page
                                       :on (:= 'argument-set.page-id 'page.id)
                                       :where (:and (:= 'argument-set.todo 1)
                                                    (:= 'page.internal in-ex)))
                              1 (random count))
                      :plist)))
	  (if plist (get-dao 'tanuki-schema:argument-set (getf plist :id)))))))

;;;
;;; Agent control: 
;;;

;; Flag to control agent lifetime.
(defparameter +loop-flag+ t
  "Let working agents know they can stop.")
(defun flag-p ()
  +loop-flag+)
(defun flag-up ()
  (setf +loop-flag+ t))
(defun flag-down ()
  (setf +loop-flag+ nil))

;; TODO: have the aset getter be an argument rather than wired.
(defun tanuki-step (agent selection-function)
  "Run an agent until it shouldn't."
  (kvetch (format nil "Starting agent ~a..." agent))
  (do ()
      ((not (flag-p)))
    (let ((aset (apply selection-function nil)))
      (if aset
          (process-argument-set agent aset)
        (progn
          (kvetch (format nil "~a is waiting for something new..." agent))
          (sleep 60)))))
  (kvetch (format nil "Agent ~a is stopping." agent)))

;; TODO: see the old runner for target selector stuff...
(defun start-a-tanuki (&optional (select-fnct #'random-undone-argument-set))
  (flag-up) ; hmmm...methinks we need read thread handling...
  (let ((agent (make-agent)))
    (bordeaux-threads:make-thread
     (lambda ()
       (with-connection
        tanuki-db:*connection-parameters*
        (tanuki-step agent select-fnct)))
     :name (symbol-name (gensym)))))

(defun stop-tanukis ()
  "..."
  (flag-down))

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
  
(defun report-general ()
  "Print the current status of current Tanuki system and return the
number of unvisited URLs."
  (let ((target (query (:select 'target :from 'meta) :single))
        (start (bb-time:date-string
                (query (:select 'start :from 'meta) :single)))
        (external-page-count (row-count 'page (:= 'internal 0)))
        (internal-page-count (row-count 'page (:= 'internal 1)))
        (aset-count (row-count 'argument-set))
        (hit-count (row-count 'hit))
        (good-count (row-count 'hit (:and (:= 0 'flagged) (:= 1 'success))))
        (flagged-count (row-count 'hit (:and (:= 1 'flagged) (:= 1 'success))))
        (error-count (row-count 'hit (:and (:= 1 'flagged) (:= 0 'success))))
        (odd-count (row-count 'hit (:and (:= 0 'flagged) (:= 0 'success)))))
    (format t "Current Tanuki status:~%")
    (format t "Base URL: ~a~%" target)
    (format t "Started at: ~a~%" start)
    (format t "Known external pages: ~a~%" external-page-count)
    (format t "Known internal pages: ~a~%" internal-page-count)
    (format t "Collected argument sets: ~a~%" aset-count)
    (format t "Attempted page visits: ~a~%" hit-count)
    (format t "Total good pages: ~a~%" good-count)
    (format t "Total flagged pages: ~a~%" flagged-count)
    (format t "Total error pages: ~a~%" error-count)
    (format t "Total odd pages: ~a~%" odd-count)))

(defun report-plists (qlist)
  (let ((count 0))
    (dolist (q qlist)
      (incf count)
      (format t "~%")
      (format t "Generic ID: ~a~%" (getf q :id))
      (format t "Argument set ID: ~a~%" (getf q :argument-set-id))
      (format t "Page ID: ~a~%" (getf q :page-id))
      (format t "Date: ~a~%" (bb-time:humanstamp (getf q :date)))
      (format t "Processing time: ~a~%" (getf q :wait))
      (format t "Code: ~a~%" (getf q :code))
      (format t "Success: ~a~%" (getf q :success))
      (format t "Flagged: ~a~%" (getf q :flagged))
      (format t "Reference: ~a~%" (getf q :reference))
      (format t "URL: ~a~%" (getf q :clean-url)))
    count))

(defun report-error ()
  "Give a report about all of the sets with an \"error\" (non-success) hit."
  (let ((qlist (query (:select '* :from 'hit
                               :inner-join 'argument-set
                               :on (:= 'hit.argument-set-id 'argument-set.id)
                               :where (:and (:= 'hit.success 0)
                                            (:= 'hit.flagged 1)))
                      :plists)))
    (report-plists qlist)))
 
(defun report-flagged ()
  "Give a report about all of the sets with a \"flagged\" hit."
  (let ((qlist (query (:select '* :from 'hit
                               :inner-join 'argument-set
                               :on (:= 'hit.argument-set-id 'argument-set.id)
                               :where (:and (:= 'hit.success 1)
                                            (:= 'hit.flagged 1)))
                      :plists)))
    (report-plists qlist)))

(defun report-marked ()
  "Give a report about all of the sets with a \"mark\"."
  (let ((qlist (query (:select '* :from 'argument-set
                               :where (:= 'argument-set.mark 1))
                      :plists)))
    (report-plists qlist)))

;;;
;;; Live management--test fixes and do reruns interactively
;;; (i.e. without resetting the database).
;;;

(defun mark-argument-set (aset?)
  "Mark an argument set and remove its hits by argument set id."
  (let ((aset (parse-argument-set aset?)))
    (when aset 
      ;; Mark found aset.
      (update-dao-value aset 'tanuki-schema:mark 1)
      ;; Remove associated hits.
      (dolist (hit (argument-set=>hit aset))
        (remove-hit hit)))))

(defun mark-problems (type)
  "Remove hits and set mark (but don't reset todo--hide from running agents)."
  (let ((success-flag 1) (flagged-flag 0))
    (cond 
     ((eq type 'error) 
      (progn (setf success-flag 0) (setf flagged-flag 1)))
     ((eq type 'flagged) 
      (progn (setf success-flag 1) (setf flagged-flag 1)))
     (t (error "need to be either 'error or 'flagged")))
    (with-transaction ()
     ;; Get arguments sets.
     (let ((hits (query-dao 'tanuki-schema:hit
                  (:select '* :from 'hit
                           :where (:and (:= 'hit.success success-flag)
                                        (:= 'hit.flagged flagged-flag))))))
       (dolist (hit hits)
         ;; Mark found asets (while removing past artifacts).
         (let ((aset (hit->argument-set hit)))
           (mark-argument-set aset)))))))

;; NOTE: No transaction here--nobody should be bothering us...
(defun rerun-marked ()
  "Rerun marked argument sets and unmark them."
  (let ((agent (make-agent))
        (asets (query-dao 'tanuki-schema:argument-set
                          (:select '* :from 'argument-set
                                   :where (:= 'argument-set.mark 1))))
        (count 0))
    (dolist (aset asets)
      (incf count)
      ;; Run as usual.
      (kvetch (format nil "Rerunning ~a of ~a..."
                      count (length asets)))
      (process-argument-set agent aset)
      ;; Unmark.
      (update-dao-value aset 'tanuki-schema:mark 0)
    count)))

(defun add-url (url)
  "Do our best to add a URL, and all things associated, to the
database."
  (let ((agent (make-agent))
        (link (make-link url)))
    (fetch agent (clean-url link))
    (process-agent agent)))
