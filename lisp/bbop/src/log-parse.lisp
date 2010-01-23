;;;; #!/usr/bin/sbcl --script
;;;;
;;;; (load "log-parse.lisp")
;;;;
;;;; ;; load an apache log file into couch
;;;; (process-to-couchdb "a2_log" "/tmp/access_log")
;;;;
;;;; ;; set logj to be list of JAR accessing Java agents
;;;; (size (setf logj (slurp-lite "/home/sjcarbon/tmp/access_log" :function #'(lambda (line) (logline line)) :test #'(lambda (r) (if (and (scan +phenote-jar-scanner+ (cdr (assoc :file r))) (scan +java-scanner+ (cdr (assoc :agent r)))) t nil)))))
;;;; ;; look at number of unique ips in logj
;;;; (size (remove-duplicates (mapcar #'(lambda (x) (cdr (assoc :ip x))) logj) :test #'equal))
;;;; ;; graph the little ones
;;;; (graph-downloads (downloads-per-month logj) "/tmp/num.png")
;;;;


(require 'cl-containers)
(require 'cl-json)
(require 'puri)
(require 'cl-ppcre)
(require 'adw-charting)
(require 'adw-charting-vecto)
(require 'toolkit)
(require 'bbop)

(defpackage :log-parse
  (:use :cl
	:cl-containers
	:json
	:cl-ppcre
	;;:adw-charting-vecto
	:adw-charting
	:toolkit-io
	:bbop-couchdb)
  (:export))

(defvar *month-three-char-strings* '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
				     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar *month-num-strings* '("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"))

(defconstant +apache-line-scanner+ (cl-ppcre:create-scanner "^([\\d.]+) (\\S+) (\\S+) \\[([\\w:/]+\\s[+\\-]\\d{4})\\] \"(.+?)\" (\\d{3}|\-) (\\d+|\-) \"([^\"]+)\" \"([^\"]+)\""))
(defconstant +apache-date-scanner+ (cl-ppcre:create-scanner "[\\s\:\/]+"))
(defconstant +whitespace-scanner+ (cl-ppcre:create-scanner "\\s+"))
(defconstant +double-return-scanner+ (cl-ppcre:create-scanner "\\n\\n"))
(defconstant +single-return-scanner+ (cl-ppcre:create-scanner "\\n"))
(defconstant +colon-space-scanner+ (cl-ppcre:create-scanner "\:\\s+"))
(defconstant +robot-useragent-scanner+ (cl-ppcre:create-scanner "^robot-useragent:\\s+"))
(defconstant +phenote-scanner+ (cl-ppcre:create-scanner "phenote\.jnlp$"))
(defconstant +phenote-jar-scanner+ (cl-ppcre:create-scanner "phenote\.jar$"))
(defconstant +java-scanner+ (cl-ppcre:create-scanner "[Jj]ava"))

(defun log-date-convert (str)
  "Convert a date sequence into a sane orderable integer rep (not seconds, eg 200908..."
  (let ((month-int (make-container 'alist-container)))
    (loop for m in *month-three-char-strings* 
       for n in *month-num-strings* 
       do (setf (item-at month-int m) n))
    (destructuring-bind (day month-str year hour minute second tz)
	(split +apache-date-scanner+ str)
      `((:year . ,year) (:month . ,(item-at month-int month-str)) (:day . ,day)
	(:hour . ,hour) (:minute . ,minute) (:second . ,second) (:timezone . ,tz)))))

(defun logline (str)
  "Returns an association list from an apache log line string."
  ;;  (register-groups-bind (ip doc)
  (register-groups-bind (ip nil nil date doc code size referer agent)
      (+apache-line-scanner+ str :sharedp t)
    ;;(format t "ip: ~A~%" ip)
    (let ((doc-parts (split +whitespace-scanner+ doc)))
      ;;      `((:ip . ,ip)))))
      `((:ip . ,ip)
	(:time . ,(log-date-convert date))
	(:method . ,(nth 0 doc-parts))
	(:file . ,(nth 1 doc-parts))
	(:protocol . ,(nth 2 doc-parts))
	(:code . ,code)
	(:size . ,size)
	(:referer . ,referer)
	(:agent . ,agent)))))


(defun downloads-per-month (log-list)
  "Return list of number of records per yearmonth as list."
  (let ((cache (make-hash-table)))
    (loop for alist in log-list 
	  do (let* ((year-str (cdr (assoc :year (cdr (assoc :time alist)))))
		    (month-str (cdr (assoc :month (cdr (assoc :time alist)))))
		    (date-num (parse-integer
			       (concatenate 'string year-str month-str))))
	       (if (null (gethash date-num cache))
		   (setf (gethash date-num cache) 0))
	       (setf (gethash date-num cache) (1+ (gethash date-num cache)))))
    (let ((final-list '()))
      (maphash #'(lambda (k v)
		   ;;(format t "~A~%" (list k v))
		   (push (list k v) final-list))
	     cache)
      final-list)))

    
(defun graph-downloads (list png-filename)
  (with-chart
   (:bar 600 400)
   (add-series "Downloads" list)
   (set-axis :y "log entries")
   (set-axis :x "year/month")
   (save-file png-filename)))


(defun process-to-couchdb (db-str file-str)
  "Returns the number of lines successfully processed."
  (size
   (slurp file-str
	  :function #'(lambda (line)
			(add-doc db-str (logline line))))))
;;			(add-doc db-str (logline line) (get-uuid))))))

;; First needs?: iconv all.txt -f Windows-1252 -t utf-8 > all-20100113.txt
;; /tmp/all-bad.txt 
(defvar *all-bad-agents-hash* nil)
(defun read-robots (file-str)
  "Returns the number of lines successfully processed."
  (setf *all-bad-agents-hash* (make-hash-table :test #'equal))
  (slurp file-str
	 :function #'(lambda (line)
		       (setf (gethash line *all-bad-agents-hash*) t))))

;; (with-open-file (stream "/tmp/all-bots.txt" :direction :output) (maphash #'(lambda (k v) (format stream "~A~%" k)) *all-agents-hash*))

(defun do-log-total (filename)
  (let ((unique-ips-hash (make-hash-table :test #'equal))
	(unique-acc 0)
	(total-acc 0))
    (slurp filename :function
	   #'(lambda (line)
	       (let* ((alist (logline line))
		      (agent-str (cdr (assoc :agent alist)))
		      (ip-str (cdr (assoc :ip alist)))
		      (year-str (cdr (assoc :year (cdr (assoc :time alist)))))
		      (month-str (cdr (assoc :month (cdr (assoc :time alist)))))
		      (file-str (cdr (assoc :file alist))))
		 (when (and (scan +phenote-scanner+ file-str)
			    (null (gethash agent-str *all-bad-agents-hash*)))
		   (setf total-acc (1+ total-acc))
		   (when (null (gethash ip-str unique-ips-hash))
		     (setf unique-acc (1+ unique-acc))
		     (setf (gethash ip-str unique-ips-hash) ip-str))))))
    (values unique-acc total-acc)))

(defun do-log-monthly (filename)
  (let ((unique-ips-hash (make-hash-table :test #'equal))
	(date-hash (make-hash-table :test #'equal))
	(unique-acc 0)
	(total-acc 0))
    (slurp filename :function
	   #'(lambda (line)
	       (let* ((alist (logline line))
		      (agent-str (cdr (assoc :agent alist)))
		      (ip-str (cdr (assoc :ip alist)))
		      (year-str (cdr (assoc :year (cdr (assoc :time alist)))))
		      (month-str (cdr (assoc :month (cdr (assoc :time alist)))))
		      (date-str (concatenate 'string year-str month-str))
		      (file-str (cdr (assoc :file alist))))
		 (when (and (scan +phenote-scanner+ file-str)
			    (null (gethash agent-str *all-bad-agents-hash*)))
		   ;; Is known date?
		   (if (null (gethash date-str date-hash))
		       (setf (gethash date-str date-hash) 0))
		   ;;(setf total-acc (1+ total-acc))
		   (when (null (gethash ip-str unique-ips-hash))
		     (setf (gethash ip-str unique-ips-hash)
			   (1+ (gethash ip-str unique-ips-hash))))))))))