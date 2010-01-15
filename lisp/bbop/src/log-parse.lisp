;;;; #!/usr/bin/sbcl --script
;;;;
;;;;
;;;;
;;;;


(require 'cl-containers)
(require 'cl-json)
(require 'cl-ppcre)
(require 'toolkit)
(require 'bbop)

(defpackage :log-parse
  (:use :cl
	:cl-containers
	:json
	:cl-ppcre
	:toolkit-io
	:bbop-couchdb)
  (:export))

(defvar *month-three-char-strings* '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
				     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar *month-num-strings* '("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"))

(defconstant +apache-line-scanner+ (create-scanner "^([\\d.]+) (\\S+) (\\S+) \\[([\\w:/]+\\s[+\\-]\\d{4})\\] \"(.+?)\" (\\d{3}|\-) (\\d+|\-) \"([^\"]+)\" \"([^\"]+)\""))
(defconstant +apache-date-scanner+ (create-scanner "[\\s\:\/]+"))
(defconstant +whitespace-scanner+ (create-scanner "\\s+"))
(defconstant +double-return-scanner+ (create-scanner "\\n\\n"))
(defconstant +single-return-scanner+ (create-scanner "\\n"))
(defconstant +colon-space-scanner+ (create-scanner "\:\\s+"))
(defconstant +robot-useragent-scanner+ (create-scanner "^robot-useragent:\\s+"))
(defconstant +phenote-scanner+ (create-scanner "phenote\.jnlp$"))

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
		       (setf (gethash date-str date-hash)) 0)
		   ;;(setf total-acc (1+ total-acc))
		   (when (null (gethash ip-str unique-ips-hash))
		     (setf (gethash ip-str unique-ips-hash)
			   (1+ (gethash ip-str unique-ips-hash))))))))))