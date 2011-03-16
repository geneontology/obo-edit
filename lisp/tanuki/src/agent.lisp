;;;; -*- mode: Lisp -*-
;;;;
;;;; Add the concepts of internal and external.
;;;;

(defpackage :tanuki-agent
  (:use
   :cl
   :kappa)
  (:export
   ;; Classes.
   tanuki-agent
   ;; Slots.
   home-url
   internal-p
   internal-links
   external-links
   ;; Super class.
   link
   ;; Super slots.
   raw-url
   clean-url
   base-url
   user-agent
   current-url
   errors
   content
   code
   links
   ;; Super methods.
   fetch
   purge
   make-link
   to-string
   authority
   query-string
   query-list
   ;; Methods.
   is-internal-p
   is-code-ok-p))
(in-package :tanuki-agent)

(defvar *version* "0.1.0" "This version of Tanuki Agent.")
(defparameter +user-agent+
  (format nil "Tanuki ~a (over CL Kappa (over Drakma))" *version*)
  "User agent string.")

;; Subclass of kappa:agent.
(defclass tanuki-agent (agent)
  ((home-url
    :documentation "String url to help judge relativity properties."
    :accessor home-url
    :initform (error "Must supply a base url (as a string) for a Tanuki agent.")
    :initarg :home-url)
   (user-agent
    :documentation "Override super's with ours."
    :initform +user-agent+)
   (internal-p
    :documentation "Whether or nor the current url is internal
    relative to the base url."
    :accessor internal-p
    :initform nil
    :initarg :internal-p)
   (internal-links
    :documentation "Internal links relative to the home-url."
    :accessor internal-links
    :initform '()
    :initarg :internal-links)
   (external-links
    :documentation "External links relative to the home-url."
    :accessor external-links
    :initform '()
    :initarg :external-links)))

(defmethod purge :after ((agent tanuki-agent))
  "We do not reset the home-url."
  (setf (internal-p agent) nil)
  (setf (internal-links agent) '())
  (setf (external-links agent) '())
  t)

;;
(defmethod fetch :after ((agent tanuki-agent) url-str)
  "Run a tanuki agent against a specified url. Make
  sure all tanuki-agents slots that can be filled are. This is a more
  more full-bodied (e.g. links) extension of mechanizes fetch that
  looks at internalness and externalness."
  (setf (internal-p agent) (is-internal-p agent url-str))
  (dolist (l (links agent))
    (if (is-internal-p agent l)
        (push l (internal-links agent))
      (push l (external-links agent))))
  t)

(defgeneric is-internal-p (tanuki-agent query-url)
  (:documentation "Check whether the current url is internal relative
  to the agent\'s home-url."))

(defmethod is-internal-p ((agent tanuki-agent) query-str)
  "Check whether the current url is internal relative to home-url."
  (let ((query-puri (puri:parse-uri
                     (kappa::make-canonical query-str
					       :relative-to (home-url agent))))
	(base-puri (puri:parse-uri (home-url agent))))
    (string= (puri:uri-authority query-puri)
	     (puri:uri-authority base-puri))))

(defgeneric is-code-ok-p (agent)
  (:documentation "Check whether the agent has an \"ok\" return code."))

(defmethod is-code-ok-p ((agent tanuki-agent))
  "..."
  (let* ((code (code agent))
         (clean-code (if (stringp code)
                         (parse-integer code)
                       code)))
    (if (> (- 400 clean-code) 0) t nil)))
