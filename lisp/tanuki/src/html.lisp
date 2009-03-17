;;;;
;;;; NOTE: This library should only Operate on strings.
;;;; NOTE: Unit tests are currently in this namespace: "(run-tests)", they
;;;;       are not exported symbols.
;;;;
;;;; TODO: need to have my own 'safe-parse-uri' function to catch the
;;;; nasty pages made by people who like to break the web
;;;;

;;(clc:clc-require :s-http-client)
(require :drakma)
(require :puri)
(require :closure-html)
(require :fiveam)
(defpackage :tanuki-html
  (:use :cl
	:drakma
	:puri
	:closure-html
	:fiveam)
  (:export
   ;;:health-check
   :fetch-doc
   ;;:pull-all-hrefs
   :extract-links-from-url
   :extract-links
   :extract-forms
   ;;:url-string-p
   :internal-p
   :render
   :render-list

   :page-is-problematic))
(in-package :tanuki-html)

;;;
;;;
;;;

(defun ok-code (code)
  "Decide if a retun code is in the OK range or not. Argument may be
a integer or a string."
  (let ((clean-code (if (stringp code)
			(parse-integer code)
		      code)))
  (if (> (- 400 clean-code) 0) t nil)))

;;;
;;; Things that may happen when working with fetch-doc
;;;

(define-condition page-is-problematic (error)
  ((problem :initarg :problem :reader problem)))
  ;;((text :initarg :text :reader text)))
;;  ((type :initarg :type :reader type)))

;; Catch both code and more serious errors.
(defun fetch-doc (url-str)
  "Fetch a document from tha interwebs. Return doc as string."
  ;(format t "___~a~%" (parse-uri str-or-puri))
  (handler-case ;; Try and catch resolution errors.
   (progn
     (sb-ext:with-timeout 600
      (multiple-value-bind (doc code struct puri dunno)
	  (http-request url-str)
	(cond
	 ;; BUG? Don't deal with images.
	 ((search "png" (cdr (assoc :CONTENT-TYPE struct))) "")
	 ((search "gif" (cdr (assoc :CONTENT-TYPE struct))) "")
	 ((ok-code code) doc)
	 (t nil)))))
   ;; BUG: this isn't cool. http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&Submit=Go&id=CP000158
   (SB-KERNEL::HEAP-EXHAUSTED-ERROR (hee)
	(error 'page-is-problematic :problem url-str))
   ;; BUG: nor is this. http://biocyc.org/META/substring-search?type=NIL&object=PARATHION-DEGRADATION-PWY"   
   (END-OF-FILE (eof)
	(error 'page-is-problematic :problem url-str))
   ;; BUG: nor this. http://flybase.bio.indiana.edu/reports/FBgn0086348.html
   ;; WARNING: things that trip this may make slime barf as well.
   (PURI:URI-PARSE-ERROR (upe)
	(error 'page-is-problematic :problem url-str))
   (sb-ext:timeout (to) nil)
   (USOCKET:TIMEOUT-ERROR (ute) nil)
   (USOCKET:HOST-UNREACHABLE-ERROR (hue) nil)
   (USOCKET:CONNECTION-REFUSED-ERROR (cre) nil)
   (SB-BSD-SOCKETS::no-address-error (nae) nil)
   (SB-KERNEL:CASE-FAILURE (cf) nil)))

;; This in an internal function and it therefore capable of working
;; with puris as well as strings.
(defun merge-with-base (url-str-list &optional (base-str-or-puri nil))
  "Takes a list of urls and a base url and merges them together to
produce fully qualified strings if possible. While the second argument
is technically optional, and the first in the list will be used in its
stead, it's probably not particularly useful behavior."
  (if url-str-list 
      (let* ((puri-list (mapcar #'parse-uri url-str-list))
	     (base-puri (parse-uri (if base-str-or-puri
				       (parse-uri base-str-or-puri)
				     (render-uri (car puri-list))))))
	(mapcar #'(lambda (e)
		    (merge-uris e base-puri))
		puri-list))))

;;
(defun render (url-str)
  "Changes a str into a proper url. This is a method canonicalization
for possibly funny URLs."
  (render-uri (parse-uri url-str) nil))

;;
(defun render-list (str-list)
  "Changes (a list of) str/urls into a list of proper urls. This is a
method canonicalization for possibly funny URLs."
  (mapcar #'render str-list))

;;
(defun internal-p (query-str base-str)
  "Check whether the first is internal to the second."
  (let ((query-puri (parse-uri query-str))
	(base-puri (parse-uri base-str)))
    (string= (uri-authority query-puri)
	     (uri-authority base-puri))))

;; TODO: Learn how to make this one with more style.
(defun pull-all-hrefs (doc-str)
  "Extract all of the hrefs as a list from an HTML string."
  (if doc-str
      (let ((parsed-document (chtml:parse doc-str (chtml:make-pt-builder)))
	    (anchor-list '()))
	(labels ((rec-dec (item)
			  (if item
			      (progn
				(if (and (string= (pt-name item) "A")
					 (getf (pt-attrs item) :HREF))
				    (push (getf (pt-attrs item) :HREF)
					  anchor-list))
				(mapcar #'rec-dec (pt-children item))))))
	  (rec-dec parsed-document))
	(mapcar (lambda (x)
		  (handler-case
		   (parse-uri x)
		   ;; BUG: maybe we need to fix it on our end and theirs http://localhost/cgi-bin/amigo_1_5_MAINTENANCE/term-details.cgi?term=GO:0018250&session_id=9216amigo1214528420
		   (PURI:URI-PARSE-ERROR (upe)
		        (error 'page-is-problematic :problem x))))
		anchor-list))))

(defun pull-all-forms (doc-str)
  "Extract all of the forms as a list from an HTML string."
  (if doc-str
      (let ((parsed-document (chtml:parse doc-str (chtml:make-pt-builder)))
	    (form-list '()))
	(labels ((rec-dec (item)
			  (if item
			      (progn
				(if (and (string= (pt-name item) "FORM")
					 (getf (pt-attrs item) :ACTION))
				    (progn
				      (print "Got form...")
				      (push (list (getf (pt-attrs item) :ACTION)
						  (getf (pt-attrs item) :ID)
						  (getf (pt-attrs item) :NAME)
						  (getf (pt-attrs item) :METHOD))
					    form-list)))
				;; Try all children.
				(mapcar #'rec-dec (pt-children item))))))
	  (rec-dec parsed-document))
	form-list)))
;; 	(mapcar (lambda (x)
;; 		  (handler-case
;; 		   (parse-uri x)
;; 		   ;; BUG: maybe we need to fix it on our end and theirs http://localhost/cgi-bin/amigo_1_5_MAINTENANCE/term-details.cgi?term=GO:0018250&session_id=9216amigo1214528420
;; 		   (PURI:URI-PARSE-ERROR (upe)
;; 		        (error 'page-is-problematic :problem x))))
;; 		form-list))))


;; Is this overly cute?
;(defun url-string-p (str)
;  "Returns true if a url string, nil otherwise."
;  (handler-case
;   (if (uri str) t)
;   (uri-parse-error (upe) nil)))

(defun extract-links-from-url (str-or-url &optional (base-str-or-url nil))
  "Returns two values, a list of internal URLs and a list of external
URLs. If you want the results to be relative to a different base other
than the argument, use the optional second argument."
  (let* ((base-url (if base-str-or-url
		       base-str-or-url
		     str-or-url))
	 (puri-list (merge-with-base (pull-all-hrefs (fetch-doc str-or-url))
				     base-url)))
    (values
     (mapcan #'(lambda (x)
		 (let ((in-url (render x)))
		   (and (internal-p in-url base-url) (list in-url))))
	     puri-list)
     (mapcan #'(lambda (x)
		 (let ((in-url (render x)))
		   (and (not (internal-p in-url base-url)) (list in-url))))
	     puri-list))))

;; TODO: no base URL makes everything internal.
(defun extract-links (str-doc base-url)
  "Returns two values, a list of internal URLs and a list of external
URLs."
  (let ((puri-list (merge-with-base (pull-all-hrefs str-doc) base-url)))
    (values
     (mapcan #'(lambda (x)
		 (let ((in-url (render x)))
		   (and (internal-p in-url base-url) (list in-url))))
	     puri-list)
     (mapcan #'(lambda (x)
		 (let ((in-url (render x)))
		   (and (not (internal-p in-url base-url)) (list in-url))))
	     puri-list))))

;; TODO: no base URL makes everything internal.
(defun extract-forms (str-doc base-url)
  "Returns two values, a list of internal forms and a list of external
forms."
  (let ((puri-list (merge-with-base (pull-all-hrefs str-doc) base-url)))
    (values
     (mapcan #'(lambda (x)
		 (let ((in-url (render x)))
		   (and (internal-p in-url base-url) (list in-url))))
	     puri-list)
     (mapcan #'(lambda (x)
		 (let ((in-url (render x)))
		   (and (not (internal-p in-url base-url)) (list in-url))))
	     puri-list))))

;;;
;;; Unit tests.
;;;
;;; TODO: It would be nice to have an web server that spins up
;;; here so we can work off of it. Maybe after the web front-end
;;; is done?
;;;

(defun run-tests()
  "General top-level test runner."
  (format t "Running ok-code tests:~&")
  (run! 'ok-code-tests)
  (format t "Running fetch-doc tests:~&")
  (run! 'fetch-doc-tests)
  (format t "Running internal-p tests:~&")
  (run! 'internal-p-tests))

;; ok-code
(test ok-code-tests
      (is (eq (ok-code 200) t)
	  "200 should be OK")
      (is (eq (ok-code 399) t)
	  "388 should be OK")
      (is (eq (ok-code 400) nil)
	  "400 should not be OK")
      (is (eq (ok-code 401) nil)
	  "401 should not be OK")
      (is (eq (ok-code "200") t)
	  "200 should be OK")
      (is (eq (ok-code "399") t)
	  "388 should be OK")
      (is (eq (ok-code "400") nil)
	  "400 should not be OK")
      (is (eq (ok-code "401") nil)
	  "401 should not be OK"))

;; fetch-doc
(test fetch-doc-tests
      (is (eq (fetch-doc "www.google") nil)
	  "Shouldn't be able to resolve \"www.google\"")
      (is (eq (fetch-doc "http://www.google") nil)
	  "Shouldn't be able to resolve \"http://www.google\"")      
      (let ((doc (fetch-doc "http://www.google.com")))
	(is (and (stringp doc) (> (length doc) 0))
	    "Couldn't fetch live doc (google)."))
      (let ((doc (fetch-doc "http://www.gnu.org")))
	(is (and (stringp doc) (> (length doc) 0))
	    "Couldn't fetch live doc (gnu).")))

;; merge-with-base
(test merge-with-base-tests
      ;; TODO
      )

;; render
(test render-tests
      ;; TODO
      )

;; internal-p
(test internal-p-tests
      (let ((base "http://localhost/my.cgi"))
	(is (eq (internal-p base base) t)
	    "Same should be internal")
	(is (eq (internal-p "/a.html" base) nil)
	    "Pieces should not be internal")
	(is (eq (internal-p "http://localhost/my.cgi/foo" base) t)
	    "Relative shift should be internal")
	(is (eq (internal-p "http://localhost" base) t)
	    "Domain should be internal")
	(is (eq (internal-p "http://localhost:9000" base) nil)
	    "Different port should not be internal")
	(is (eq (internal-p "http://localhost:80" base) t)
	    "Same port should be internal")
	(is (eq (internal-p "http://foo.localhost/my.cgi" base) nil)
	    "Different subdomain should not be internal")
	(is (eq (internal-p "http://externalhost/my.cgi" base) nil)
	    "Different domain should not be internal")))

;; pull-all-hrefs
(test pull-all-hrefs-tests
      ;; TODO
      )

;; ;; url-string-p
;; ;; TODO: not very interesting, why do I even have this function?
;; (test url-string-p-tests
;;       (is (eq (url-string-p "") t)
;; 	  "Nothing is a URL.")
;;       (is (eq (url-string-p "nope://foo") t)
;; 	  "Unpleasant but true.")
;;       (is (eq (url-string-p "foo") t)
;; 	  "2.")
;;       (is (eq (url-string-p "/foo") t)
;; 	  "3.")
;;       (is (eq (url-string-p "http://") nil)
;; 	  "4 Not a complete URL.")
;;       (is (eq (url-string-p "www.foo.com") t)
;; 	  "5.")
;;       (is (eq (url-string-p "http://localhost") t)
;; 	  "6.")
;;       (is (eq (url-string-p "http://www.foo.com") t)
;; 	  "7.")
;;       (is (eq (url-string-p "http://www.foo.commmmmmmm") t)
;; 	  "8."))

;; extract-links-from-url
(test extract-links-from-url-tests
      ;; TODO
      )
