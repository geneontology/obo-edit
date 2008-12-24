;;;;
;;;; Common utilities and tricks...implemented yet again...
;;;;

(defpackage :toolkit-conv
  (:use :cl)
  (:export :plist-keys
	   :mklist
	   :flatten
	   :filter
	   :ccat))
(in-package :toolkit-conv)


(defun plist-keys (plist)
  "Retrieve a list of plist keys."
  (if plist
      (append (list (car plist))
	      (plist-keys (cddr plist)))))

(defun mklist (obj)
  "Make a list from an object if it is a list."
  (if (listp obj) obj (list obj)))

(defun flatten (x)
  "Flatten nested lists."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro ccat (&rest strings)
  "Concatenate a set of strings."
  `(concatenate 'string ,@strings))

(defun date-string (utime)
  "Return a date string representing universal time."
  (let ((day-names '("Monday" "Tuesday" "Wednesday" "Thursday"
		     "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
	(decode-universal-time utime)
      (format nil "~2,'0d:~2,'0d:~2,'0d on ~a, ~d/~2,'0d/~d (GMT~@d)"
	      hour
	      minute
	      second
	      (nth day-of-week day-names)
	      month
	      date
	      year
	      (- tz)))))


(defmacro key-check (message &rest keys)
  ""
  `(when (not (and ,@keys))
     (error ,message)))

;;; Started writing my own keyword handler...

(defun list-even-p (list)
  "Is the length of the list even?"
  (if (evenp (length list)) t nil))

(defun list-is-all-keywords-p (list)
  "Are all of the items in the list keywords?"
  (every #'keywordp list))

(defun list-has-no-keywords-p (list)
  "Are none of the items in the list keywords?"
  (notany #'keywordp list))

(defun in-list (list &optional (funct #'evenp))
  "Returns a sublist whose items meet some criteria relative to its
position in the list; takes a single int arg function (e.g. evenp,
oddp, etc.). Defaults to evenp."
  (let ((i 0))
    (mapcan
     #'(lambda (x) (incf i)
	 (when (funcall funct i) (list x)))
     list)))

(defun action-on (&rest list)
  "pseudo &key--pairs keywords and their arguments into a list"
  (when (not (list-even-p list))
    (error "bad length of args (not even)"))
  (let ((keywords (in-list list #'oddp))
	(keyargs (in-list list)))
    (if (not (and (list-is-all-keywords-p keywords)
		  (list-has-no-keywords-p keyargs)))
	(error "mixed keywords and arguments")
      (loop
       for k in keywords
       for a in keyargs
       collect (list k a)))))

;; (defmacro filter (list &rest keys)
;;   ""
;;   `(when (not (and ,@keys))
;;      (error ,message)))
(defun filter (list &rest funcs)
  ""
  (let ((funx (flatten funcs))) ; nested lists on recursion
    (if (car funx)
	(progn
	  (filter (remove-if (car funx) list) (cdr funx)))
	list)))

(defun dig (&rest foo)
  (print foo))
