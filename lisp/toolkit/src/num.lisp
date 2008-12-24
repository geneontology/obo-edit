;;;;
;;;; Functions for parameterizing form inputs.
;;;;


(defpackage :toolkit-num
  (:use :cl)
  (:export :integer-with-range
	   :float-with-range
	   :shuffle))
(in-package :toolkit-num)


(defun integer-with-range (lo hi)
  "Returns an integer including the lo and hi."
  (+ lo (random (1+ (- hi lo)))))

(defun float-with-range (lo hi)
  "Returns an integer including the lo and hi."
  (+ lo (random (- hi lo))))

(defun random-from-list (list)
  "Returns a random item from a list."
  (if list (elt list (random (length list)))))

;; TODO: flag for string or file.
(defun random-set (type)
  "Returns a file string for a random file in directory type in the
data directory."
  nil)

;; TODO: figure out how this really works.
; got this as snippet from somewhere...
;; TODO: this should be called "shuffle!"
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
;;; Math
;;;

(defun z-random (&optional (n 10))
  "Generate a random number between 0 and n."
  (random (+ n 1)))

(defun nz-random (&optional (n 10))
  "Generate a random number between 1 and n inclusive."
  (1+ (random n) ))

(defun nazo118 ()
  "Solution to nazo 118."
  (let ((probs (loop
		  for x from 26 downto 1
		  for y from 52 downto 27
		  collect (/ x y))))
    (print probs)
    (apply #'* probs)))

