#!/usr/bin/sbcl --script
;;;;
;;;;
;;;;
;;;;

(require 'metatilities)
;;(require 'cl-containers)
;;(require 'cl-ppcre)

;; Byte-banging.
;;(let ((re (cl-ppcre:create-scanner "[^A-Za-z]")))

(defun bytes-from-stream (stream)
  "Return a list of bytes from a file arg."
  (let ((l nil))
    (do ((byte (read-byte stream nil)
	       (read-byte stream nil)))
	((null byte))
      (setf l (nconc l (cons byte nil))))
    l))

(defun bytes-from-file (arg)
  (with-open-file (stream arg :element-type 'unsigned-byte)
    (bytes-from-stream stream)))

(defun ascii-control-byte-p (b)
  (if (or (< b 32) (> b 126)) t nil))

;; (filter-values #'(lambda (x) (if (not (ascii-control-byte x)) x nil)))
				 
    
;;  (let ((l (make-container 'list-container)))
    ;;(print-container l)))
;;    (print-container l)))


;; (setf l' (0 1 2 3 4 5 6 7 8 9))
;; > (0 1 2 3 4 5 6 7 8 9)
;; (splice-in! l 1 4 '(a b c))
;; > (0 A B C 9)
;; TODO: Make sure this is as fast and destructive as possible.
(defun splice-in! (list new-seg seg-start seg-end)
  (let ((insert (copy-list new-seg))
	(back (nthcdr seg-end list))
	(front (nbutlast list (- (length list) seg-start))))
    (nconc front insert back)))

;; 0 0 4 32 0 [X]
(defun hard-replace! (list target-seg replace-seg)
  (let ((target-seg-length (length target-seg))
	;(replace-seg-length (length replace-seg))
	(accu 0)
	(index 0))
    (loop do
	 (progn 
	   (setf index (search target-seg list
			       :start1 0
			       :end1 target-seg-length
			       :start2 accu
			       :end2 (length list)))
	   (if index
	       (progn 
		 ;(format t "~%index: ~A~%" index)
		 ;(format t "accu: ~A~%" accu)
		 ;(format t "before: ~A~%" list)
		 (splice-in! list replace-seg index (+ index target-seg-length))
		 ;(format t "after: ~A~%" list)
		 (setf accu (+ index target-seg-length))
		 ;(format t "index: ~A~%" index)
		 ;(format t "accu: ~A~%" accu)
		 (if (> accu (length list)) (setf index nil)))))
       until (not index)))
  list)

(defparameter *byte-change-maps*
;;  '())
  '(((0 0 4 32 0) . ())
    ((0 0 5 32 0) . ())))
;;   ((0 0) . ())))

(defun main (arg)
  (let ((byte-list (bytes-from-file arg)))
    (loop for map in *byte-change-maps*
       do (progn
	    ;; (filter-values
	    ;;  #'(lambda (x) (if (not (ascii-control-byte x)) x nil))
	    ;;  byte-list)
	    (hard-replace! byte-list (car map) (cdr map))))    
    (format t "~A" (coerce (mapcar #'code-char byte-list) 'string))))

;; Command iteration "loop".
;(format t "~A~%" (car *posix-argv*))
;(format t "~A~%" (sb-ext:posix-getenv "_"))
;;(mapcar #'main (cdr *posix-argv*))
