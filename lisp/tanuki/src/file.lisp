;;;;
;;;;
;;;;

(clc:clc-require :cl-ppcre)
(defpackage :tanuki-file
  (:use :cl
	:cl-ppcre)
  (:export :file-exists-p
	   :warehouse
	   :name-to-warehouse
	   :check-warehouse))
(in-package :tanuki-file)

(defparameter +tanuki-warehouse-location+ (truename ".")
  "The location of the data warehouse on the filesystem.")

;;;
;;; Filesystem.
;;;

(defun file-exists-p (pathname)
  "Check for the existance of a file, returns the pathname if not nil."
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  
  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
  
  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented on this lisp"))

(defun pwd ()
  "Return the current working directory."
  #+(or sbcl)
  (truename ".")
  #-(or sbcl)
  (error "pwd not implemented"))

;; BUG: that hard-code is a buggy buggy bug.
(defun cd (&optional (path "/home/sjcarbon" path-supplied-p))
  "Change the current working directory."
  #+(or sbcl)
  ;;(if (path-supplied-p)
  (setf *default-pathname-defaults* (truename path))
  ;;    (setf *default-pathname-defaults* (truename path)))
  #-(or sbcl)
  (error "get-cwd not implemented"))

;;;
;;; DB on filesystem.
;;;

;; TODO: a little too lispy for my taste.
(defun warehouse (&rest segments)
  "Getter/setter for the data warehouse location. Returns nil if not extant."
  (let ((location (if segments
		      (setf +tanuki-warehouse-location+
			    (make-pathname :directory
					   (cons :absolute segments)))
		    +tanuki-warehouse-location+)))
    (if (file-exists-p location)
	location
      nil)))

(defun name-to-warehouse (name)
  "Turn a name into a usable db filename and log filename."
   (merge-pathnames (concatenate 'string (substitute #\_ #\Space
						     (string-trim
						      '(#\Space) name))
				 ".tdb")
		    (warehouse)))

(defun name-to-warehouse (name)
  "Turn a name into a usable db filename and log filename."
  (let ((mangled-str (substitute #\_ #\Space (string-trim '(#\Space) name))))
    (values 
     (merge-pathnames (concatenate 'string mangled-str ".tdb") (warehouse))
     (merge-pathnames (concatenate 'string mangled-str ".log") (warehouse)))))

(defun check-warehouse (id)
  ""
  (let ((dbfile (file-exists-p (name-to-warehouse id))))
    (if dbfile dbfile nil)))

;;;
;;; File parsing.
;;;

;; Can't use '#' or '/' cause that might actually be real URLs in some
;; case in the future. Will use ';'.  And yes, it could be done easier
;; (like with a single regexp), but I find breaking it down this way
;; easier to read.
(defvar *ws-scanner* (create-scanner "^\\s*$")
  "Detect a whitespace string.")
(defvar *comment-scanner* (create-scanner "^\\s*\;")
  "Detect a comment line.")
(defvar *url-scanner* (create-scanner "^\\s*([^\\s]+)\\s*")
  "Extract yummy center.")
;;(defparameter *url-scanner* (create-scanner "^\\s*([^\\s]+)\\s*$")
;;  "Extract yummy center.")

(defun extract-string (str)
  "Grab the first nice-looking string on a line."
  (cond
    ((= (length str) 0) nil) ; is a string
    ((scan *comment-scanner* str) nil) ; not a comment
    ((scan *ws-scanner* str) nil) ; not just ws
    (t ;; 
     (register-groups-bind (meat)
	 (*url-scanner* str)
       meat))))

;; Open file and return list of URLs.
;; Note: the first version of this was *much* longer.
(defun slurp-file (filename)
  "Slurp in a file and return URL looking thingies as a list of strings."
  (with-open-file (in filename :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil)
	 while line
	 when (extract-string line) collect it))))
