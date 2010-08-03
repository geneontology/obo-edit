;;;; -*- mode: Lisp -*-
;;;;
;;;; Assuming good access for user:
;;;; createdb -h localhost -U tanuki_user -W tanuki
;;;;
;;;; Access:
;;;; psql -W -U tanuki_user -h localhost tanuki
;;;;
;;;; From the beginning:
;;;;    (require 'tanuki)
;;;;    (in-package :tanuki)
;;;;    (setf t1 (make-tanuki-system :start-url "http://localhost/cgi-bin/amigo/amigo" :data-location "/tmp" :dbname "tanuki" :dbuser "tanuki_user" :dbpass "tanuki_pass" :dbhost "localhost"))
;;;;    (initialize-database t1) ; TODO: untested
;;;;    (start-a-tanuki t1 'no-argument-set)
;;;;    (start-a-tanuki t1 'random-undone-argument-set)
;;;;    (stop-tanukis)
;;;;
;;;; Continue:
;;;;    (setf t1 (make-tanuki-system :start-url "http://localhost/cgi-bin/amigo/amigo" :data-location "/tmp" :dbname "tanuki" :dbuser "tanuki_user" :dbpass "tanuki_pass" :dbhost "localhost"))
;;;;    (start-a-tanuki 'distant-undone-argument-set)
;;;;    (stop-tanukis)
;;;;
;;;; WISHLIST:
;;;; *) Create real database from inside.
;;;; *) Form detection, profiling, and running.
;;;; *) Fuzzing and input (link and form) generation.
;;;; *) Toy frontend extension language with safe function calls as API.
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
  (:import-from :tanuki-db-op
                :row-count
                :table-plist
                :update-dao-value)
  ;; (:import-from :tanuki-schema
  ;;               :url
  ;;               :page-id
  ;;               :page
  ;;               :todo
  ;;               :internal)
  )
(in-package :tanuki)

;;;
;;; REPL-based debugging.
;;;

(defun debug-sql (sym)
  "Takes 'on or 'off."
  (setf cl-postgres:*query-log* (if (eq sym 'on) *standard-output* nil)))

(defun debug-http (sym)
  "Takes 'on or 'off."
  (setq drakma:*header-stream* (if (eq sym 'on) *standard-output* nil)))

;;;
;;; ...
;;;

(defmacro with-db-from (tsys &body body)
  "..."
  `(with-connection (tanuki-db:parameters (db ,tsys)) ,@body))

(defclass tanuki-system ()
  ((label
    :documentation "..."
    :accessor label
    :initform (concatenate 'string "tanuki_" (symbol-name (gensym)))
    :initarg :label)
  (start-url
    :documentation "The default target used when not explicitly defined."
    :accessor start-url
    :initform "http://localhost/"
    :initarg :start-url)
   (data-location
    :documentation "The default data location used when not explicitly defined."
    :accessor data-location
    :initform "/tmp/data"
    :initarg :data-location)
   (db
    :documentation "The internal database connection."
    :accessor db
    :initform nil)   
   (random-type
    :documentation "The type of random generation to be used: 'weighed, 'internal, 'external, and 'random."
    :accessor random-type
    :initform 'weighed)
   (sample-size
    :documentation "The size of the random sample in the database for each iteration."
    :accessor sample-size
    :initform 33)
   (sample-truncate
    :documentation "..." 
    :accessor sample-truncate
    :initform nil)
   (stop-list
    :documentation "List of tanuki agent thread names we want stopped."
    :accessor stop-list
    :initform '())
   (wait-time
    :documentation "Time in seconds to wait before trying the database for a new target after failing to find one."
    :accessor wait-time
    :initform 60)))

(defun make-tanuki-system (&key (label nil) (start-url nil) (data-location nil)
                                (dbname nil) (dbuser nil)
                                (dbpass nil) (dbhost nil))
  "You really do have to use this to make one: (eq (doing-it-by-hand) :bad)"
  (let ((ts (make-instance 'tanuki-system)))
    (when start-url (setf (slot-value ts 'start-url) label))
    (when start-url (setf (slot-value ts 'start-url) start-url))
    (when data-location (setf (slot-value ts 'data-location) data-location))
    (setf (slot-value ts 'db) (tanuki-db:make-tanuki-database
                               :dbname dbname :dbuser dbuser
                               :dbpass dbpass :dbhost dbhost))
    ts))

(defmethod kvetch ((ts tanuki-system) obj)
  "Wrapper; with stdout (for interactive operations) too."
  (format t "~a~%" obj)
  (with-db-from ts
   (let ((new-message (make-instance 'tanuki-schema:message
                                     :id (sequence-next 'message-id-seq)
                                     :source (label ts)
                                     :date (bb-time:timestamp)
                                     :text (format nil "~a" obj))))
     (insert-dao new-message))))

(defmethod make-agent ((ts tanuki-system))
  "Quick agent maker for tanuki."
  (make-instance 'tanuki-agent :home-url (start-url ts)))

(defmethod initialize-database ((ts tanuki-system))
  "Only needs to be run once ever after creating a new ts. Can be used for both initialization and wiping/cleaning."
  (tanuki-db:reset-database (db ts) (start-url ts) (data-location ts)))

;; ;; TODO: Move to bbop.
;; (defun %runner (program &optional (args '()))
;;   "..."
;;   (let ((output-stream
;;          (sb-ext:process-output
;;           (sb-ext:run-program program args :search t :wait t :output :stream))))
;;     (with-open-stream
;;      (s output-stream)
;;      (loop for line = (read-line s nil nil)
;;            while line
;;            collect line into lines
;;            counting t into line-count
;;            finally (return (values lines line-count))))))

;; ;; TODO: could be better.
;; (defun fresh-pg-database (&key (dbname "tanuki") (dbuser "")
;;                                (dbpass "") (dbhost "localhost"))
;;   "createdb -h localhost -U tanuki_user -W tanuki"
;;   (%runner "createdb" '("-h" dbhost "-U" dbuser "-W" dbname)))

;;;
;;; REPL report queries.
;;;

(defmethod report-general ((ts tanuki-system))
  "Print the current status of current Tanuki system and return the
number of unvisited URLs."
  (with-db-from ts
   (let ((target (query (:select 'target :from 'meta) :single))
         (start (bb-time:date-string
                 (query (:select 'start :from 'meta) :single)))
         (external-page-count (row-count 'page (:= 'internal 0)))
         (internal-page-count (row-count 'page (:= 'internal 1)))
         (aset-count (row-count 'argument-set))
         (hit-count (row-count 'hit))
         (good-count (row-count 'hit (:and (:= 0 'flagged) (:= 1 'success))))
         (n-flagged (row-count 'hit (:and (:= 1 'flagged) (:= 1 'success))))
         (n-error (row-count 'hit (:and (:= 1 'flagged) (:= 0 'success))))
         (odd-count (row-count 'hit (:and (:= 0 'flagged) (:= 0 'success)))))
     (format t "Current Tanuki status:~%")
     (format t "Base URL: ~a~%" target)
     (format t "Started at: ~a~%" start)
     (format t "Known external pages: ~a~%" external-page-count)
     (format t "Known internal pages: ~a~%" internal-page-count)
     (format t "Collected argument sets: ~a~%" aset-count)
     (format t "Attempted page visits: ~a~%" hit-count)
     (format t "Total good pages: ~a~%" good-count)
     (format t "Total flagged pages: ~a~%" n-flagged)
     (format t "Total error pages: ~a~%" n-error)
     (format t "Total odd pages: ~a~%" odd-count))))
  
(defun %report-plists (qlist)
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

(defmethod report-error ((ts tanuki-system))
  "Give a report about all of the sets with an \"error\" (non-success) hit."
  (with-db-from ts
   (let ((qlist (query (:select '* :from 'hit
                                :inner-join 'argument-set
                                :on (:= 'hit.argument-set-id 'argument-set.id)
                                :where (:and (:= 'hit.success 0)
                                             (:= 'hit.flagged 1)))
                       :plists)))
     (%report-plists qlist))))
 
(defmethod report-flagged ((ts tanuki-system))
  "Give a report about all of the sets with a \"flagged\" hit."
  (with-db-from ts
   (let ((qlist (query (:select '* :from 'hit
                                :inner-join 'argument-set
                                :on (:= 'hit.argument-set-id 'argument-set.id)
                                :where (:and (:= 'hit.success 1)
                                             (:= 'hit.flagged 1)))
                       :plists)))
     (%report-plists qlist))))

(defmethod report-marked ((ts tanuki-system))
  "Give a report about all of the sets with a \"mark\"."
  (with-db-from ts
   (let ((qlist (query (:select '* :from 'argument-set
                                :where (:= 'argument-set.mark 1))
                       :plists)))
     (%report-plists qlist))))

;;;
;;; Functions for target selection.
;;;

;;
(defmethod no-argument-set ((ts tanuki-system))
  "Returns nothing. Can be used for testing tanuki controls."
  nil)

;; select * from argument_set inner join page on (argument_set.page_id = page.id) where argument_set.todo = 1 and page.internal = 1;
(defmethod random-undone-argument-set ((ts tanuki-system) &optional (number-of-sets 1))
  "Get a random unvisted page as a DAO, otherwise nil. Takes the
symbols: 'internal, 'external, 'weighed, or 'random."
  (let* ((rttype (random-type ts))
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

;; Built on top of random-undone-argument-set.
(defmethod distant-undone-argument-set ((ts tanuki-system))
  ;; Get a sample and remove the dupes.
  (let* ((sample-set 
          (remove-duplicates
           (random-undone-argument-set ts (sample-size ts))
           :test (lambda (x y)
                   (equal (tanuki-schema:clean-url x)
                          (tanuki-schema:clean-url y)))))
         ;; Call decide using clean url.
         (wanted-url (decide (mapcar (lambda (x)
                                       (tanuki-schema:clean-url x)) sample-set)
                             :truncate (sample-truncate ts))))
    ;; Fish the wanted argument set out of the list using the wanted
    ;; url.
    (find-if (lambda (x) (equal wanted-url (tanuki-schema:clean-url x)))
             sample-set)))

;;;
;;; Controlling agents and agents' threads.
;;;

(defun this-thread-name ()
  "The name of the thread this executes in."
  (bordeaux-threads:thread-name (bordeaux-threads:current-thread)))

(defmethod tanuki-main-loop ((ts tanuki-system) agent selection-function)
  "Run an agent until it shouldn't."
  (kvetch ts (format nil "Starting agent ~a..." agent))
  (do ()
      ;; The do exit condition is checking for self on stop-list.
      ((find (this-thread-name) (stop-list ts) :test #'string=))
    (let ((aset (apply selection-function ts nil)))
      (if aset
          (process-argument-set ts agent aset)
        (progn
          (kvetch ts (format nil "~a is waiting for something new (~as.)..."
                          (this-thread-name) (wait-time ts)))
          (sleep (wait-time ts))))))
  ;; Remove self from stop list (if on) and halt.
  (setf (stop-list ts) (remove (this-thread-name)
                               (stop-list ts) :test #'string=))
  (kvetch ts (format nil "Agent ~a is stopping." (this-thread-name))))

(defmethod start-a-tanuki ((ts tanuki-system) select-fnct)
  "..."
  (let ((agent (make-agent ts)))
    (bordeaux-threads:make-thread
     (lambda ()
       (with-db-from ts
        (tanuki-main-loop ts agent select-fnct)))
     :name (bb-util:ccat
            (label ts) "/"
            (string-downcase (symbol-name select-fnct)) "/"
            (symbol-name (gensym))))))

(defmethod running-tanukis ((ts tanuki-system))
  "Return a list of the names of the running tanuki threads."
  (mapcar #'bordeaux-threads:thread-name
          (remove-if (lambda (x) ()
                       (not (cl-ppcre:scan
                             (bb-util:ccat "^" (label ts) "/")
                             (bordeaux-threads:thread-name x))))
                     (bordeaux-threads:all-threads))))

(defmethod stop-tanuki ((ts tanuki-system) tt-name)
  "Add all running tanukis to the stop list."
  (pushnew tt-name (stop-list ts) :test 'string=))

(defmethod stop-tanukis ((ts tanuki-system))
  "Add all running tanukis to the stop list."
  (dolist (rt (running-tanukis ts)) (stop-tanuki ts rt))
  (stop-list ts))

;;;
;;; Operations: changing values in the database.
;;;

;;
(defmethod process-argument-set ((ts tanuki-system) agent aset)
  "Run an agent with argument set, adding the results to the
database."
  (with-db-from ts
   (kvetch ts (format nil "~a looking at: ~a" agent
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
         (process-agent ts agent))))))

(defmethod process-agent ((ts tanuki-system) agent)
  "Take the contents of an agent and add it to the database as
necessary."
  (with-db-from ts
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
             ))))))))

;;;
;;; Live management--test fixes and do reruns interactively
;;; (i.e. without resetting the database).
;;;

(defmethod mark-argument-set ((ts tanuki-system) aset?)
  "Mark an argument set and remove its hits by argument set id."
  (with-db-from ts
   (let ((aset (parse-argument-set aset?)))
     (when aset 
       ;; Mark found aset.
       (update-dao-value aset 'tanuki-schema:mark 1)
       ;; Remove associated hits.
       (dolist (hit (argument-set=>hit aset))
         (remove-hit hit))))))

(defmethod mark-problems ((ts tanuki-system) type)
  "Remove hits and set mark (but don't reset todo--hide from running agents)."
  (with-db-from ts
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
                                      :where (:and (:= 'hit.success
                                                       success-flag)
                                                   (:= 'hit.flagged
                                                       flagged-flag))))))
        (dolist (hit hits)
          ;; Mark found asets (while removing past artifacts).
          (let ((aset (hit->argument-set hit)))
            (mark-argument-set ts aset))))))))
  
;; NOTE: No transaction here--nobody should be bothering us...
(defmethod rerun-marked ((ts tanuki-system))
  "Rerun marked argument sets and unmark them."
  (with-db-from ts
   (let ((agent (make-agent ts))
         (asets (query-dao 'tanuki-schema:argument-set
                           (:select '* :from 'argument-set
                                    :where (:= 'argument-set.mark 1))))
         (count 0))
     (dolist (aset asets)
       (incf count)
       ;; Run as usual.
       (kvetch ts (format nil "Rerunning ~a of ~a..." count (length asets)))
       (process-argument-set ts agent aset)
       ;; Unmark.
       (update-dao-value aset 'tanuki-schema:mark 0)
       count))))

(defmethod add-url ((ts tanuki-system) url)
  "Do our best to add a URL, and all things associated, to the
database."
  (let ((agent (make-agent ts))
        (link (make-link url)))
    (fetch agent (clean-url link))
    (process-agent ts agent)))
