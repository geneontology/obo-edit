;;;; -*- mode: Lisp -*-
;;;;
;;;; NOTE:
;;;;

(defpackage :tanuki-decide
  (:use :cl :alexandria)
  (:export :levenshtein-distance
	   :mutual-mean-distance
	   :decide))
(in-package :tanuki-decide)

;;
(defun string-window (str &key (window (length str)) (offset 0))
  "Safely return a substring within a window."
  (let ((str-len (length str)))
    (if (> offset window) ""
	(subseq str offset (if (> window str-len) str-len window)))))

;; Mostly nicked from public domain file based on the Scheme Wikipedia
;; version.
(defun levenshtein-distance (s1 s2 &key (window -1 use-window-p))
  "Return the Levenshtein distance between two strings."
  ;; When the window is defined, trim both string to that size when
  ;; doing calculations.
  (when use-window-p
    (setf s1 (string-window s1 :window window))
    (setf s2 (string-window s2 :window window)))
  ;; Initing.
  (let* ((width (1+ (length s1)))
         (height (1+ (length s2)))
         (d (make-array (list height width))))
    (dotimes (x width) (setf (aref d 0 x) x))
    (dotimes (y height) (setf (aref d y 0) y))
    ;; Calc.
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

(defun mutual-mean-distance (sample &key (trim nil))
  "Return the string in the sample that is the least like the
others. Option to trim all strings down to minimum size During calculation."
  ;; An int array of size sample set to 0s.
  (let ((store (make-array (list (length sample) (length sample))
			   :element-type (type-of 2)
			   :initial-element 0))
	(all-pairs (pair-set sample))
	(str-to-int-hash (make-hash-table :test 'equal))
	(int-to-str-hash (make-hash-table))
	(min-length (length (car sample))))
    ;; Set the hash tables with a string/int mapping. Also, find
    ;; minimum str length while we're at it.
    (let ((i 0))
      (dolist (str sample)
	(when (< (length str) min-length) (setf min-length (length str)))
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
	     (l-dist (if trim 
			 (levenshtein-distance x-str y-str :window min-length)
			 (levenshtein-distance x-str y-str))))
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


;; We're looking for the greatest average distance in the set.
(defun decide (sample &key (truncate nil))
  "Return a single string from the sample list that is most
  \"different\" from the background list."
  ;; Get the average distances.
  (let* ((mm-list (mutual-mean-distance sample :trim truncate))
	 ;; Pull out the maximums.
	 (a-max (cdr (car mm-list)))
	 (good-list (remove-if (lambda (x) (not (eq a-max (cdr x)))) mm-list)))
    ;; Select a random one,
    (car (random-elt good-list))))

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
