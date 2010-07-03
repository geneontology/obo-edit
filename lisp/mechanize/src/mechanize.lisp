;;;; -*- mode: Lisp -*-
;;;;
;;;; Web agent based loosely on WWW::Mechanize.
;;;;
;;;; TODO: Maybe make links (and forms) classes of their own.
;;;;
;;;; URLs known to trip failure:
;;;;    http://flybase.bio.indiana.edu/reports/FBgn0086348.html
;;;;    http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&Submit=Go&id=CP000158
;;;;    http://biocyc.org/META/substring-search?type=NIL&object=PARATHION-DEGRADATION-PWY"   
;;;;
;;;; For full arguments, see:
;;;; http://common-lisp.net/~loliveira/ediware/drakma/request.lisp
;;;;
;;;; NOTE: I decided closure-html is a pain in the neck--the parse
;;;; tree is now handled by hand.
;;;;
;;;; TODO/LATER: - forms and arguments
;;;;

(defpackage :mechanize
  (:use
   :cl
   :drakma)
  (:export
   ;; Classes.
   agent
   ;; Slots.
   current-url
   errors
   content
   code
   links
   ;; Methods.
   fetch
   purge
   ))
(in-package :mechanize)

(defparameter +agent-timeout+ 300
  "Force a waiting timeout after this many seconds.")
(defparameter +make-canonical+ t
  "Try and make found links correct relative to the current url.")

;;;
;;; NOTE: Change the parameters of the URI parse in puri--there are a
;;; lot of sites that refuse to follow the RFCs, but one has to work
;;; with them...
;;;

(setf puri:*strict-parse* nil) ; looks like it's in export
(defparameter puri::*illegal-characters*
  (puri::reserved-char-vector
   (remove #\| (remove #\# puri::*excluded-characters*))))
(defparameter puri::*strict-illegal-query-characters*
  (puri::reserved-char-vector
   (append '(#\?) (remove #\# puri::*excluded-characters*))))
(defparameter puri::*illegal-query-characters*
  (puri::reserved-char-vector
   puri::*excluded-characters* :except '(#\^ #\| #\#)))

(defclass agent ()
  ((current-url
    :accessor current-url
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
    :initform '())
   (forms ; TODO: implement this...
    :accessor links
    :initform '())))

(defgeneric fetch (agent url-str)
  (:documentation "Run a tanuki agent against a specified url. This is
  mostly to get the document body and check for errors. For a more
  broad use (e.g. links), see run."))

;;
(defmethod fetch ((agent agent) url-str)
  "..."
  ;;
  (purge agent)
  (setf (current-url agent) url-str)
  (labels ((glitch (err-str)
                   (setf (errors agent) (cons err-str (errors agent)))
                   nil))
    ;; Try and use drakma in a minimal way and catch resolution errors
    ;; (with the above function).
    (handler-case
     (progn
       (sb-ext:with-timeout +agent-timeout+
         (multiple-value-bind
             (body response-code headers puri stream must-close-p reason)
             (http-request url-str :redirect 100)
           (declare (ignore puri stream must-close-p reason))
           (cond
            ;; TODO: Doesn't deal with images real well yet.
            ((search "png" (cdr (assoc :CONTENT-TYPE headers))) "")
            ((search "gif" (cdr (assoc :CONTENT-TYPE headers))) "")
            (t (fill-out-agent-with-body agent body)))
           (setf (code agent) response-code)
	   ;; Return if we made it without an error.
	   t)))
    (END-OF-FILE (eof)
     (declare (ignore eof))
     (glitch "EOF--reached end of file?"))
    (PURI:URI-PARSE-ERROR (upe)
     (declare (ignore upe))
     (glitch "uri parse error"))
    (CHUNGA:SYNTAX-ERROR (se)
     (declare (ignore se))
     (glitch "chunga: syntax error"))
    (USOCKET:TIMEOUT-ERROR (ute)
     (declare (ignore ute))
     (glitch "server(?) timeout"))
    (USOCKET:HOST-UNREACHABLE-ERROR (hue)
     (declare (ignore hue))
     (glitch "host/server unreachable"))
    (USOCKET:CONNECTION-REFUSED-ERROR (cre)
     (declare (ignore cre))
     (glitch "host/server connection refused"))
    #+(or sbcl)
    (sb-ext:timeout (to)
     (declare (ignore to))
     (glitch "client timeout"))
    #+(or sbcl)
    (SB-KERNEL::HEAP-EXHAUSTED-ERROR (hee)
     (declare (ignore hee))
     (glitch "heap exhausted--page too big?"))
    #+(or sbcl)
    (SB-BSD-SOCKETS::NO-ADDRESS-ERROR (nae)
     (declare (ignore nae))
     (glitch "no (such?) address"))
    #+(or sbcl)
    (SB-BSD-SOCKETS:NAME-SERVICE-ERROR (nse)
     (declare (ignore nse))
     (glitch "name service error: could not resolve DNS?"))
    #+(or sbcl)
    (SB-KERNEL:CASE-FAILURE (cf)
     (declare (ignore cf))
     (glitch "case failure--what is this?")))))

(defgeneric purge (agent)
  (:documentation "Try and get an agent back to a \"pristine\" state"))

(defmethod purge ((agent agent))
  (setf (current-url agent) nil)
  (setf (errors agent) '())
  (setf (content agent) nil)
  (setf (code agent) nil)
  (setf (links agent) '())
  t)

;;;
;;; Unexported helper functions.
;;;

;;
(defun fill-out-agent-with-body (agent body)
  "Things done if we get a proper text body."
  (setf (content agent) body)
  (setf (links agent) (extract-links body))
  (when +make-canonical+
    (setf (links agent)
	  (mapcar (lambda (x)
		    (make-canonical x :relative-to (current-url agent)))
		  (links agent)))))

(defun make-rational (url-str)
  "Changes a url string into a proper url one way or another. This is
a cleaner for possibly odd, but mostly correct, urls."
  (puri:render-uri (puri:parse-uri url-str) nil))

(defun make-canonical (url-str &key (relative-to))
  "This merges the incoming url string with a relative url string. It
tries to make sense of partial urls, such as in links. Should lead to
some interesting bugs..."
  (make-rational
   (puri:merge-uris (puri:parse-uri url-str) (puri:parse-uri relative-to))))

;; Move along the list and pull out likely triplets.
(defun scan-out-links (xlist)
  "Worthless without a flattened tree from chtml:parse."
  (let ((hrefs '())
        (len (length xlist)))
    (when (>= len 3)
      (dotimes (i (- len 3))
	(let ((1st  (elt xlist i))
	      (2nd (elt xlist (+ 1 i)))
	      (3rd (elt xlist (+ 2 i))))
          (if (and (eq :A 1st) (eq :HREF 2nd)
                   (stringp 3rd))
              (push 3rd hrefs)))))
    hrefs))

(defun extract-links (doc)
  "Hackily extract links strings from an HTML document."
  (when doc
    (let ((flattened-tree (bb-util:flatten
                           (chtml:parse doc (chtml:make-lhtml-builder)))))
      (scan-out-links flattened-tree))))

;;;
;;; Testing.
;;; TODO: better testing of everything...
;;;

;; (defun test-extract-links ()
;;   (let ((doc "<p><a href = \"/foo\">blah</a></p>"))
;;     (extract-links doc)))
