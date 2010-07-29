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
;;;;    (start-a-tanuki #'distant-undone-argument-set)
;;;;    [tail -f /tmp/tanuki.log]
;;;;    (stop-tanukis)
;;;;
;;;; Partial:
;;;;    (setf a (make-agent))
;;;;    (process-argument-set a (random-undone-argument-set))
;;;;
;;;; WISHLIST:
;;;; *) Add "action" table to keep track of tanuki life events.
;;;; *) Class-ify tanuki so we can work our way towards a web-based
;;;;    general interface for multiple sites running in parallel. Mostly
;;;;    this means moving parameters into slots.
;;;; *) Form detection, profiling, and running.
;;;; *) Fuzzing and input (link and form) generation.
;;;; *) Toy frontend extension language with safe function calls as
;;;;    API.
;;;; *) Web interface.
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

(defun debug-sql (sym)
  "Takes 'on or 'off."
  (setf cl-postgres:*query-log* (if (eq sym 'on) *standard-output* nil)))

(defun debug-http (sym)
  "Takes 'on or 'off."
  (setq drakma:*header-stream* (if (eq sym 'on) *standard-output* nil)))

(defun kvetch (obj)
  "Wrapper; with stdout (for interactive operations) too."
  (format nil "~a~%" obj)
  (bb-log:kvetch +default-logger+ obj))

(defun make-agent ()
  "Quick agent maker."
  (make-instance 'tanuki-agent :home-url *default-url*))

(defun top-reset (new-default-url new-default-data)
  "Blows away everything for a fresh start with the given url."
  (setf *default-url* new-default-url)
  (setf *default-data* new-default-data)
  (tanuki-db:reset-database *default-url* *default-data*)
  t)

(defun top-remember ()
  "Try to get ready using the database."
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
  "Run an agent with argument set, adding the results to the
database."
  (kvetch (format nil "~a looking at: ~a" agent
                  (tanuki-schema:clean-url aset)))
  (when aset
    (update-dao-value aset 'tanuki-schema:todo 0) ; aset is now off todo list
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
  "Take the contents of an agent and add it to the database as
necessary."
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

;;
(defun no-argument-set ()
  "Returns nothing. Can be used for testing tanuki controls."
  nil)

;; Option to control the type of target that we're looking at.
(defparameter +random-target-type+ 'weighed
  ;; 'internal 'external 'weighed 'random
  "Choose the type of target to select when looking for a new random
  argument set. Can be: 'internal, 'external, 'weighed, or 'random.")

;; select * from argument_set inner join page on (argument_set.page_id = page.id) where argument_set.todo = 1 and page.internal = 1;
(defun random-undone-argument-set (&optional (number-of-sets 1))
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
	(let ((plists
               (loop for d from 1 to number-of-sets
                     collect
                     (query (:limit (:select '*
                                             :from 'argument-set
                                             :inner-join 'page
                                             :on (:= 'argument-set.page-id
                                                     'page.id)
                                             :where (:and (:= 'argument-set.todo
                                                              1)
                                                          (:= 'page.internal
                                                              in-ex)))
                                    1 (random count))
                            :plist))))
	  (when plists
            (let ((sets (mapcar (lambda (plist)
                                  (get-dao 'tanuki-schema:argument-set
                                           (getf plist :id)))
                                plists)))
              (if (= number-of-sets 1) (car sets) sets)))))))

(defparameter +sample-size+ 33 ; down from 100
  "The size of the random sample in the database for each iteration.")
(defparameter +sample-truncate+ nil
  "...")

;; Built on top of random-undone-argument-set.
(defun distant-undone-argument-set ()
  ;; Get a sample and remove the dupes.
  (let* ((sample-set 
          (remove-duplicates
           (random-undone-argument-set +sample-size+)
           :test (lambda (x y)
                   (equal (tanuki-schema:clean-url x)
                          (tanuki-schema:clean-url y)))))
         ;; Call decide using clean url.
         (wanted-url (decide (mapcar (lambda (x)
                                       (tanuki-schema:clean-url x))
                                     sample-set) :truncate +sample-truncate+)))
    ;; Fish the wanted argument set out of the list using the wanted
    ;; url.
    (find-if (lambda (x) (equal wanted-url (tanuki-schema:clean-url x)))
             sample-set)))

;;;
;;; Agent control: 
;;;

;; Flag to control agent lifetime.
(defparameter +stop-list+ '()
  "List of tanuki agent thread names we want stopped.")

(defparameter +wait-time+  60
  "Time in seconds to wait before trying the database for a new target
  after failing to find one.")

(defun this-thread-name ()
  "The name of the thread this executes in."
  (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))

(defun tanuki-stepper (agent selection-function)
  "Run an agent until it shouldn't."
  (kvetch (format nil "Starting agent ~a..." agent))
  (do ()
      ;; Check for self on stop list.
      ((find (this-thread-name) +stop-list+ :test #'string=))
    (let ((aset (apply selection-function nil)))
      (if aset
          (process-argument-set agent aset)
        (progn
          (kvetch (format nil "~a is waiting for something new (~as.)..."
                          (this-thread-name) +wait-time+))
          (sleep +wait-time+)))))
  ;; Remove self from stop list (if on) and halt.
  (setf +stop-list+ (remove (this-thread-name)  +stop-list+ :test #'string=))
  (kvetch (format nil "Agent ~a is stopping." (this-thread-name))))

(defun start-a-tanuki (&optional (select-fnct #'random-undone-argument-set))
  "..."
  (let ((agent (make-agent)))
    (bordeaux-threads:make-thread
     (lambda ()
       (with-connection
        tanuki-db:*connection-parameters*
        (tanuki-stepper agent select-fnct)))
     :name (bb-util:ccat
            "tanuki/" (string-downcase (symbol-name select-fnct))
            "/" (symbol-name (gensym))))))

(defun running-tanukis ()
  "Return a list of the names of the running tanuki threads."
  (mapcar #'bordeaux-threads:thread-name
          (remove-if (lambda (x) ()
                       (not (cl-ppcre:scan "^tanuki/"
                                           (bordeaux-threads:thread-name x))))
                     (bordeaux-threads:all-threads))))

(defun stop-tanuki (tt-name)
  "Add all running tanukis to the stop list."
  (pushnew tt-name +stop-list+ :test 'string=))

(defun stop-tanukis ()
  "Add all running tanukis to the stop list."
  (dolist (rt (running-tanukis)) (stop-tanuki rt))
  +stop-list+)

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
