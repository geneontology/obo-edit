;;;; -*- mode: Lisp -*-
;;;;
;;;; Take this over during transition--will functionally replace
;;;; tanuki-html.
;;;;
;;;; URLs known to trip failure:
;;;;    1) http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&Submit=Go&id=CP000158
;;;;    2) http://biocyc.org/META/substring-search?type=NIL&object=PARATHION-DEGRADATION-PWY"   
;;;;    3) http://flybase.bio.indiana.edu/reports/FBgn0086348.html
;;;;
;;;; http://common-lisp.net/~loliveira/ediware/drakma/request.lisp
;;;; for full arguments...
;;;;
;;;; TODO/LATER: - forms and arguments
;;;;
;;;; I decided closure-html is a pain in the neck--the parse tree is
;;;; now handled by hand.
;;;;

(require :drakma)
(require :puri)
(require :closure-html)
(defpackage :tanuki-agent
  (:use :cl
	:drakma
	:puri)
  (:export run
           fetch
           purge
	   tanuki-agent))
(in-package :tanuki-agent)


(defparameter +default-target+ "http://localhost"
  "The default (and easily redefinable) target of the agents. It is
  necessary for all of the comparative functionality.")

;; NOTE: Since this functions almost the same as the way I have the
;; class definition setup, it's not really necessary.
(defun create-agent (&optional (base-url +default-target+))
  (let ((agent (make-instance 'tanuki-agent :base-url base-url)))
    agent))

(defclass tanuki-agent ()
  ((base-url
    :accessor base-url
    :initform (or +default-target+ (error "Must supply a base url."))
    :initarg :base-url)
   (target-url
    :accessor target-url
    :initform nil)
   (internal-p
    :accessor internal-p
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

(defgeneric fetch (tanuki-agent url-str)
  (:documentation "Run a tanuki agent against a specified url. This is
  mostly to get the document body and check for errors. For a more
  broad use (e.g. links), see run."))

;;
(defmethod fetch ((agent tanuki-agent) url-str)
  "..."
  ;;
  (labels ((glitch (err-str)
                   (setf (errors agent) (cons err-str (errors agent)))
                   nil))
    ;; Catch resolution errors (with the above function)
    (handler-case
     (progn
       ;;(sb-ext:with-timeout 300 ; five minute timeout
       (sb-ext:with-timeout 5 ; five second timeout for testing
         (multiple-value-bind
             (body response-code headers puri stream must-close-p reason)
             (http-request url-str :redirect 100)
           (declare (ignore puri stream must-close-p reason))
           (cond
            ;; TODO/BUG? Don't deal with images.
            ((search "png" (cdr (assoc :CONTENT-TYPE headers))) "")
            ((search "gif" (cdr (assoc :CONTENT-TYPE headers))) "")
            ;;((ok-code code) body)
            (t
             (setf (content agent) body)
             (setf (code agent) response-code)
             t)))))
    ;; Ex: 1
    (SB-KERNEL::HEAP-EXHAUSTED-ERROR (hee)
     (declare (ignore hee))
     (glitch "heap exhausted--page too big?"))
    ;; Ex: 2
    (END-OF-FILE (eof)
     (declare (ignore eof))
     (glitch "EOF--reached end of file?"))
    ;; Ex: 3; WARNING: things that trip this may make slime barf as well.
    (PURI:URI-PARSE-ERROR (upe)
     (declare (ignore upe))
     (glitch "uri parse error"))
    (sb-ext:timeout (to)
     (declare (ignore to))
     (glitch "client timeout"))
    (USOCKET:TIMEOUT-ERROR (ute)
     (declare (ignore ute))
     (glitch "server(?) timeout"))
    (USOCKET:HOST-UNREACHABLE-ERROR (hue)
     (declare (ignore hue))
     (glitch "host/server unreachable"))
    (USOCKET:CONNECTION-REFUSED-ERROR (cre)
     (declare (ignore cre))
     (glitch "host/server connection refused"))
    (CHUNGA:SYNTAX-ERROR (se)
     (declare (ignore se))
     (glitch "chunga: syntax error"))
    (SB-BSD-SOCKETS::NO-ADDRESS-ERROR (nae)
     (declare (ignore nae))
     (glitch "no (such?) address"))
    (SB-BSD-SOCKETS:NAME-SERVICE-ERROR (nse)
     (declare (ignore nse))
     (glitch "name service error;;could not resolve DNS?"))
    (SB-KERNEL:CASE-FAILURE (cf)
     (declare (ignore cf))
     (glitch "case failure--what is this?")))))

(defgeneric run (tanuki-agent target-url)
  (:documentation "Run a tanuki agent against a specified url. Make
  sure all tanuki-agents slots that can be filled are. This is a more
  more full-bodied (e.g. links) extrension of fetch."))

;; TODO: remember to shadow +default-target+ with the one in the
;; instance.
(defmethod run ((agent tanuki-agent) target-url-str)
  "..."
  (setf (target-url agent) target-url-str)
  (setf (internal-p agent)
        (is-internal-p target-url-str :relative (base-url agent)))
  (fetch agent target-url-str)
  ;; Remove links and 
  (let ((links (extract-links (content agent))))
    (setf (links agent)
          (mapcar (lambda (x)
                    (make-canonical x :relative (base-url agent)))
                  links)))
  t)

(defgeneric purge (tanuki-agent)
  (:documentation "Try and get an agent back to a \"pristine\" state"))

(defmethod purge ((agent tanuki-agent))
  (setf (target-url agent) nil)
  (setf (internal-p agent) nil)
  (setf (errors agent) '())
  (setf (content agent) nil)
  (setf (code agent) nil)
  (setf (links agent) '())
  t)

;;;
;;; Unexported helper functions.
;;;

(defun make-rational (url-str)
  "Changes a str into a proper url one way or another. This is a
cleaner for possibly funny urls."
  (render-uri (parse-uri url-str) nil))

(defun make-canonical (url-str &key (relative +default-target+))
  "This merges the default-target parameter with the incoming string,
tries to make sense of relativity and such. Should lead to some
interesting bugs..."
  (make-rational
   (merge-uris (parse-uri url-str) (parse-uri relative))))

(defun is-internal-p (query-str &key (relative +default-target+))
  "Check whether the argument is internal relative to default-target."
  (let ((query-puri (parse-uri (make-canonical query-str :relative relative)))
	(base-puri (parse-uri relative)))
    (string= (uri-authority query-puri)
	     (uri-authority base-puri))))

(defun ok-code (code)
  "Decide if a retun code is in the OK range or not. Argument may be
a integer or a string."
  (let ((clean-code (if (stringp code)
			(parse-integer code)
			code)))
    (if (> (- 400 clean-code) 0) t nil)))

(defun flatten (xtree)
  "Flatten a tree into a list."
  (labels ((rec (xtree acc)
             (cond ((null xtree) acc)
                   ((atom xtree) (cons xtree acc))
                   (t (rec (car xtree) (rec (cdr xtree) acc))))))
    (rec xtree nil)))

;; Move along the list and pull out likely triplets.
(defun scan-out-links (xlist)
  "Worthless without a flattened tree from chtml:parse."
  (let ((hrefs '())
        (len (length xlist)))
    (if (>= len 3)
        (dotimes (i (- len 3))
          (let ((1st  (elt xlist i))
                (2nd (elt xlist (+ 1 i)))
                (3rd (elt xlist (+ 2 i))))
          (if (and (eq :A 1st)
                   (eq :HREF 2nd)
                   (stringp 3rd))
              (push 3rd hrefs)))))
    hrefs))

(defun extract-links (doc)
  "Hackily extract links strings from an HTML document." 
  (let ((flattened-tree (flatten (chtml:parse doc (chtml:make-lhtml-builder)))))
    (scan-out-links flattened-tree)))

;;;
;;; Testing.
;;; TODO/BUG: better testing of everything.
;;;

(defun test-extract-links ()
  (let ((doc "<p><a href = \"/foo\">blah</a></p>"))
    (extract-links doc)))
