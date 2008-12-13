;;;;
;;;; Usage:
;;;;  (load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki-decide.lisp")
;;;;

(clc:clc-require :fiveam)
(defpackage :tanuki-decide
  (:use :cl
	:fiveam)
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
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
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

;; Not that useful in practice: on a big enough background
(defun max-distance (str background)
  "Return the maximum distance of str against the background list."
  (apply #'max (loop for bg-str in background
		     collect (levenshtein-distance str bg-str))))

(defun average (num-list)
  "Return the average of a number list."
  (/ (apply #'+ num-list) (length num-list)))
(defun average-distance (str background)
  "Return the maximum distance of str against the background list."
  (average (loop for bg-str in background
		 collect (levenshtein-distance str bg-str))))

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

;; TODO/BUG: in the end, eventually we're going to have to do some
;; kind of iterative process (or a neural net) to make sure that we
;; get a balance of depth and breadth; however, breadth is more
;; important.

;; TODO:
(defun string-window (str window-size)
  "return a substring within a window"
  (let ((len (length str)))
    (subseq str 0 (if (> window-size len) len window-size))))

(defun length-bounds (str-list)
  "returns the upper and lower bounds of sequeunce lenghts in a list."
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

;;;
;;; Unit tests.
;;;
(defun run-tests()
  "General top-level test runner."
  (format t "Running levenshtein-distance tests:~&")
  (run! 'levenshtein-distance-tests)
  (format t "Running decide tests:~&")
  (run! 'decide-tests))

;; levenshtein-distance
(test levenshtein-distance-tests
      ""
      (is (zerop (levenshtein-distance "kitten" "kitten")) "Equals")
      (is (= (levenshtein-distance "kitten" "") 6) "Against nothing")
      (is (= (levenshtein-distance "kitten" "sitting") 3) "Similar"))

;; decide
(test decide-tests
      ""
      (is (string= (decide '("yyy" "aaa" "pdq" "xxy")
			   '("xxx" "xxy" "aab" "yyx" "bab")) 
		   "pdq") "Should find \"pdq\""))
