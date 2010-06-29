;;;;
;;;; Take this over during transition--will functionally replace
;;;; tanuki-html.
;;;;

(require :drakma)
(require :puri)
(defpackage :tanuki-agent
  (:use :cl
	:drakma
	:puri)
  (:export run
	   tanuki-agent))
(in-package :tanuki-agent)


;; TODO: make a nice and stateful web agent class (a la Mechanize).

(defun ok-code (code)
  "Decide if a retun code is in the OK range or not. Argument may be
a integer or a string."
  (let ((clean-code (if (stringp code)
			(parse-integer code)
			code)))
    (if (> (- 400 clean-code) 0) t nil)))

;;
(defun fetch-page (url-str)
  "..."
  ;;(format t "___~a~%" (parse-uri str-or-puri))
  (handler-case ; catch resolution errors
      (progn
	(sb-ext:with-timeout 300 ; five minute timeout
	  ;; See
	  ;; http://common-lisp.net/~loliveira/ediware/drakma/request.lisp
	  ;; for full arguments...
	  (multiple-value-bind (doc code struct puri dunno)
	      (http-request url-str :redirect 100)
	    (cond
	      ;; BUG? Don't deal with images.
	      ((search "png" (cdr (assoc :CONTENT-TYPE struct))) "")
	      ((search "gif" (cdr (assoc :CONTENT-TYPE struct))) "")
	      ((ok-code code) doc)
	      (t nil)))))
    ;; BUG: this isn't cool:
    ;; http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&Submit=Go&id=CP000158
    (SB-KERNEL::HEAP-EXHAUSTED-ERROR (hee)
      (error 'page-is-problematic :problem url-str))
    ;; BUG: nor is this. 
    ;; http://biocyc.org/META/substring-search?type=NIL&object=PARATHION-DEGRADATION-PWY"   
    (END-OF-FILE (eof)
      (error 'page-is-problematic :problem url-str))
    ;; BUG: nor this: http://flybase.bio.indiana.edu/reports/FBgn0086348.html
    ;; WARNING: things that trip this may make slime barf as well.
    (PURI:URI-PARSE-ERROR (upe)
      (error 'page-is-problematic :problem url-str))
    (sb-ext:timeout (to) nil)
    (USOCKET:TIMEOUT-ERROR (ute) nil)
    (USOCKET:HOST-UNREACHABLE-ERROR (hue) nil)
    (USOCKET:CONNECTION-REFUSED-ERROR (cre) nil)
    (CHUNGA:SYNTAX-ERROR (se) nil)
    (SB-BSD-SOCKETS::NO-ADDRESS-ERROR (nae) nil)
    (SB-BSD-SOCKETS:NAME-SERVICE-ERROR (nse) nil)
    (SB-KERNEL:CASE-FAILURE (cf) nil)))

;; TODO
;; init against base-url-string
;; consume url-string
;;   - internal-p
;;   - error-p
;;   - error-message
;;   - code
;;   - content
;;   - links (may have to do rendering for full canonical form)
;;   [LATER: - forms and arguments]

(defparameter +default-target+ "http://localhost"
  "The default (and easily redefinable) target of the agents.")

(defun create-agent (&optional (base-url "http://localhost"))
  (let ((agent (make-instance 'tanuki-agent :base-url base-url)))
    agent))

(defclass tanuki-agent ()
  ((base-url
    :accessor base-url
    :initform (or +default-target+ (error "Must supply a base url."))
    :initarg :base-url)
   (target-url
    :initform nil)
   (internal
    :accessor internal
    :initform nil)
   (errors
    :accessor errors
    :initform '())
   (content
    :accessor content
    :initform nil)
   (code
    :accessor code
    :initform nil)
   (links
    :accessor links
    :initform '())))

;; (setf foo (create-agent))
;; (base-url foo)

(defgeneric run (tanuki-agent target-url)
  (:documentation "Run a tanuki agent against a specified url."))

(defmethod run ((agent tanuki-agent) target-url)
  (print target-url)
  (print (base-url agent))
  ;; TODO: internal check here.
  ;;(print (slot-value agent 'base-url))
  t)


(defun internal-p (query-str base-str)
  "Check whether the first is internal to the second."
  (let ((query-puri (parse-uri query-str))
	(base-puri (parse-uri base-str)))
    (string= (uri-authority query-puri)
	     (uri-authority base-puri))))
