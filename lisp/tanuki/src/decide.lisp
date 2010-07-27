;;;; -*- mode: Lisp -*-
;;;;
;;;; NOTE:
;;;;

(defpackage :tanuki-decide
  (:use :cl :alexandria)
  (:export :levenshtein-distance
	   :decide))
(in-package :tanuki-decide)

;; Nicked from public domain file based on the Scheme Wikipedia
;; version.
(defun levenshtein-distance (s1 s2)
  "Return the Levenshtein distance between two strings."
  (let* ((width (1+ (length s1)))
         (height (1+ (length s2)))
         (d (make-array (list height width))))
    ;; Initing.
    (dotimes (x width) (setf (aref d 0 x) x))
    (dotimes (y height) (setf (aref d y 0) y))
    ;;
    (dotimes (x (length s1))
      (dotimes (y (length s2))
        (setf (aref d (1+ y) (1+ x))
              (min (1+ (aref d y (1+ x)))
                   (1+ (aref d (1+ y) x))
                   (+ (aref d y x)
                      (if (char= (aref s1 x) (aref s2 y))
                          0
                          1))))))
    (aref d (1- height) (1- width))))

;; CLtL
(defun pair-set (list)
  "List of every pair combination in a list."
  (mapcon #'(lambda (x)
	      (mapcar #'(lambda (y) (list (car x) y))
		      (cdr x)))
	  list))

;; (defun ordered-pair-list (list)
;;   "Returns a list of "

(defun mutual-mean-distance (sample)
  "Return the string in the sample that is the least like the others."
  ;; An int array of size sample set to 0s.
  (let ((store (make-array (list (length sample) (length sample))
			   :element-type (type-of 2)
			   :initial-element 0))
	(all-pairs (pair-set sample))
	(str-to-int-hash (make-hash-table :test 'equal))
	(int-to-str-hash (make-hash-table)))
    ;; Set the hash tables with a string/int mapping.
    (let ((i 0))
      (dolist (str sample)
	(setf (gethash str str-to-int-hash) i)
	(setf (gethash i int-to-str-hash) str)
	(incf i)))
    ;; Small error test.
    (when (not (eq (length sample) (length (hash-table-keys str-to-int-hash))))
      (error "There is a mismatch between table and sample..."))
    ;; Calculate the levenshtein listance across all pairs.
    (dolist (pair all-pairs)
      (let* ((x-str (first-elt pair))
	     (y-str (last-elt pair))
	     (x-coord (gethash x-str str-to-int-hash))
	     (y-coord (gethash y-str str-to-int-hash))
	     (l-dist (levenshtein-distance x-str y-str)))
	(setf (aref store x-coord y-coord) l-dist)
	(setf (aref store y-coord x-coord) l-dist)))
    ;; TODO: Find the row(s) with the highest mean.
    (let ((tally-list '()))
      (dotimes (i (length sample))
	(let ((num-list '()))
	  (dotimes (j (length sample))
	    (push (aref store i j) num-list))
	  (push (cons (gethash i int-to-str-hash)
		      (mean num-list)) tally-list)))
      (sort tally-list (lambda (x y) (> (cdr x) (cdr y)))))))


;; We're looking for the greatest average distance against the
;; background set.
(defun decide (sample background)
  "Return a single string from the sample list that is most
  \"different\" from the background list."
  ;; TODO: Random take if more than one at same distance.
  (cdar
   (sort (loop for str in sample
	       collect (cons (window-average-distance str background) str))
	 #'(lambda (x y)
	     (> (car x) (car y))))))

;; TODO:
(defun string-window (str window-size)
  "return a substring within a window"
  (let ((len (length str)))
    (subseq str 0 (if (> window-size len) len window-size))))

(defun length-bounds (str-list)
  "Returns the upper and lower bounds of sequeunce lengths in a list."
  (values
   (apply #'min (mapcar #'length str-list))
   (apply #'max (mapcar #'length str-list))))

(defun window-average-distance (str background)
  "Return the maximum distance of str against the background list."
  (let ((shortest-length (length-bounds (cons str background))))
    (average (loop for bgstr in background
		   collect (levenshtein-distance
			    (string-window str shortest-length)
			    (string-window bgstr shortest-length))))))

;; TODO: Move these elsewhere.
;; ;;;
;; ;;; Unit tests.
;; ;;;
;; (defun run-tests()
;;   "General top-level test runner."
;;   (format t "Running levenshtein-distance tests:~&")
;;   (run! 'levenshtein-distance-tests)
;;   (format t "Running decide tests:~&")
;;   (run! 'decide-tests))

;; ;; levenshtein-distance
;; (test levenshtein-distance-tests
;;       ""
;;       (is (zerop (levenshtein-distance "kitten" "kitten")) "Equals")
;;       (is (= (levenshtein-distance "kitten" "") 6) "Against nothing")
;;       (is (= (levenshtein-distance "kitten" "sitting") 3) "Similar"))

;; ;; decide
;; (test decide-tests
;;       ""
;;       (is (string= (decide '("yyy" "aaa" "pdq" "xxy")
;; 			   '("xxx" "xxy" "aab" "yyx" "bab")) 
;; 		   "pdq") "Should find \"pdq\""))
