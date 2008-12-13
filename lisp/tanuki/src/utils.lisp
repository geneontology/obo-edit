
(defpackage :tanuki-utils
  (:use :cl)
  (:export :shuffle
	   :rseq
	   :date-string
	   :random-sequence
	   :key-check))
(in-package :tanuki-utils)


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

;; TODO: figure out how this really works.
(defun shuffle (sequence)
  "Destructively shuffles a sequence."
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1.0)))
                       sequence)
                  #'< :key #'cdr)))

;; TODO: Can be faster? I chose this for the time being because it's
;; deterministic--it's probably faster when the numbers are small or
;; the range is close to the num. This should change depending on the
;; two numbers.
(defun rseq (num)
  "Returns a randomized sequence of the first num numbers."
  (shuffle (loop for i from 1 to num collect i)))

(defun nseq (num)
  "Returns a sequence of the first num numbers."
  (loop for i from 1 to num collect i))

;; TODO: This may be reimplemented faster--possibly random generation
;; checked on a hash to prevent dupes.
(defun random-sequence (num &key (squeeze nil) (range (list 1 num)))
  "Return a random sequence of NUM unique numbers (within a range if
specified)."
  ;;
  (let* ((lower-bound (car range))
	 (upper-bound (cadr range))
	 (full-range (1+ (- upper-bound lower-bound))))
    ;; Allow for getting a narrower set.
    (let ((num (if (and squeeze (> num full-range)) full-range num)))
      (when (> lower-bound upper-bound)
	(error "Bad bounds."))
      (when (> num full-range)
	(error "You may use :squeeze to narrow to the range size."))
      ;; Create a random list of size range.
      (cond
       ;; For "small sets", get the random sequence deterministically.
       ((> (* num num) full-range)
	(let ((full-seq (mapcar #'(lambda(x) (+ x (- lower-bound 1)))
				(rseq full-range))))
	  (loop
	   for item in full-seq
	   for n from num downto 1
	   collect item)))
       (t
	;; Otherwise, get is randomly. This should actually be pretty
	;; safe.
	(mapcar #'(lambda(x) (+ x (- lower-bound 1)))
		(r-seq-gen num full-range)))))))

(defun r-seq-gen (number-of-grabs seq-length)
  "Grab number-of-grabs number from a sequence of seq-length with no
repeats. If the number of grabs is too close to the seq-length, you
may be in danger. See the limits above."
  (let ((number-hash (make-hash-table)))
    (labels ((add-to-cache (number-list)
	       (if (>= (length number-list) number-of-grabs)
		   number-list
		 (let ((random-number (random seq-length)))
		   (if (gethash random-number number-hash)
		       (add-to-cache number-list)
		     (progn
		       (setf (gethash random-number number-hash) t)
		       (add-to-cache (cons random-number number-list))))))))
      (add-to-cache '()))))

;; TODO: the above is more elegant than what we had before, but I
;; still want it to sing.
(defun r-seq-gen-beta (number-of-grabs seq-length)
  "Grab number-of-grabs number from a sequence of seq-length with no
repeats. If the number of grabs is too close to the seq-length, you
may be in danger. See the limits above. BETA VERSION."
  (let ((number-hash (make-hash-table)))
    (labels ((add-to-cache (number-list)
	       (if (>= (length number-list) number-of-grabs)
		   number-list
		 (let ((random-number (random seq-length)))
		   (if (gethash random-number number-hash)
		       (add-to-cache number-list)
		     (progn
		       (setf (gethash random-number number-hash) t)
		       (add-to-cache (cons random-number number-list))))))))
      (add-to-cache '()))))
;;(defun foo (n)
;;  (if (length


;;;
;;; Utilities
;;;

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
