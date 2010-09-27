;;;;
;;;;
;;;; http://aws.amazon.com/s3/#pricing
;;;; http://aws.amazon.com/ec2/#pricing
;;;;

;; Useful definitions.
(defvar *weeks-per-month* 4.2)
(defvar *hours-per-month* (* 1.0 24.0 31.0))
(defvar *seconds-per-month* (* 1.0 60.0 60.0 24.0 31.0))

;; LBL variables.
(defparameter +lbl-contract-months+ 12.0)

;; Amazon global charges.
(defvar *aws-gb-xfer-out-charge* 0.150)
(defvar *aws-gb-xfer-in-charge* 0.100)

;;;
;;; S3
;;;

;; Amazon S3 charges.
(defvar *aws-s3-gb-store-charge* 0.165)
(defvar *aws-s3-1000-op-charge* 0.011)

;; BBOP S3 variables.
(defparameter +bbop-s3-stored-data-gb+ 100.0)
(defparameter +bbop-s3-ops-per-day+ 1000.0)

;; Calc.
(defun bbop-s3-calc ()
  (let (;; 1000 "expensive" ops per day every day.
	(ops-charge (* (/ (* +bbop-s3-ops-per-day+ 31.0 +lbl-contract-months+)
			  1000.0)
		       *aws-s3-1000-op-charge*))
	;; Stored a data is always max.
	(store-charge (* +lbl-contract-months+ +bbop-s3-stored-data-gb+
			 *aws-s3-gb-store-charge*))
	;; Let's say that half is moved in every month and half moved out.
	(xfer-charge (+ (* (/ +bbop-s3-stored-data-gb+ 2.0)
			   +lbl-contract-months+
			   *aws-gb-xfer-in-charge*)
			(* (/ +bbop-s3-stored-data-gb+ 2.0)
			   +lbl-contract-months+
			   *aws-gb-xfer-in-charge*))))
    (+ ops-charge store-charge xfer-charge)))

;;;
;;; EC2 and EBS
;;;

;; Amazon EC2 and EBS charges:
;; Standard prices.
;; | name          |  mem | comp         | price |
;; |---------------+------+--------------+-------|
;; | micro         | .613 | 2*           | 0.025 |
;; | std small     |  1.7 | 1            | 0.095 |
;; | hi-cpu small  |  1.7 | 5 (2*2.5)    |  0.19 |
;; | std large     |  7.5 | 4 (2*2)      |  0.38 |
;; | hi-mem small  | 17.2 | 6.5 (2*3.25) |  0.57 |
;; | std xlarge    |   15 | 8 (4*2)      |  0.76 |
;; | hi-cpu large  |  7   | 20 (8*2.5)   |  0.76 |
;; | hi-mem large  | 34.2 | 13 (4*3.25)  |  1.14 |
;; | hi-mem xlarge | 68.4 | 26 (8*3.25)  |  2.28 |
;;
;; Lower prices with per-buy (reserved). 
;; | name          |  mem | comp         | price | year cover |
;; |---------------+------+--------------+-------+------------|
;; | micro         | .613 | 2*           |  0.01 |         54 |
;; | std small     |  1.7 | 1            |  0.04 |     227.50 |
;; | hi-cpu small  |  1.7 | 5 (2*2.5)    |  0.08 |        455 |
;; | std large     |  7.5 | 4 (2*2)      |  0.16 |        910 |
;; | hi-mem small  | 17.2 | 6.5 (2*3.25) |  0.24 |       1325 |
;; | std xlarge    |   15 | 8 (4*2)      |  0.32 |       1820 |
;; | hi-cpu large  |    7 | 20 (8*2.5)   |  0.32 |       1820 |
;; | hi-mem large  | 34.2 | 13 (4*3.25)  |  0.48 |       2650 |
;; | hi-mem xlarge | 68.4 | 26 (8*3.25)  |  0.96 |       5300 |
;;
;; EBS numbers:
(defvar *aws-ebs-gb-store-charge* 0.11)
(defvar *aws-ebs-1000000-io-charge* 0.11)

;; We'll base our look on two machines:
;;   1) a standard small and a standard
;;   2) run 24x7
;;   3) 5GB images
;;   4) restarted once a week (image i/o)
;;   5) every month, they move their image size in and out as standard traffic
;;   6) with a 10GB EBS each for backing and database. 
;;   7) every month, they move their store size in and out as standard traffic
;;   8) for EBS, i/o request is 100 per second (continuous and ridiculous)

;;
(defparameter +bbop-ec2-image-size-gb+ 5.0)
(defparameter +bbop-ebs-store-gb+ 10.0)
(defparameter +bbop-ebs-io-tps+ 10.0) ; interesting variable to play with

;; Calculate io and storage (data) costs.
(defun bbop-ec2-ebs-data-calc (num-machines)
  (* num-machines +lbl-contract-months+ ;; machines over contract
     (+
      ;; #4
      (* *weeks-per-month* +bbop-ec2-image-size-gb+ *aws-gb-xfer-in-charge*)
      ;; #5
      (* +bbop-ec2-image-size-gb+ *aws-gb-xfer-in-charge*)
      (* +bbop-ec2-image-size-gb+ *aws-gb-xfer-out-charge*)
      ;; #6
      (* +bbop-ebs-store-gb+ *aws-ebs-gb-store-charge*)
      ;; #7
      (* +bbop-ebs-store-gb+ *aws-gb-xfer-in-charge*)
      (* +bbop-ebs-store-gb+ *aws-gb-xfer-out-charge*)
      ;; #8
      (* (/ (* *seconds-per-month* +bbop-ebs-io-tps+) 1000000.0)
	 *aws-ebs-1000000-io-charge*))))

;; Monthly
(defun bbop-ec2-cpu-calc (machine-list)
  (cond 
    ((atom machine-list) 0.0)
    (t
     (+ (let ((machine-entry (car machine-list)))
	  (+ (* +lbl-contract-months+ *hours-per-month*
		(getf machine-entry :hourly))
	     (getf machine-entry :initial)))
	(bbop-ec2-cpu-calc (cdr machine-list))))))

;;;
;;; 
;;;

(defparameter +bbop-machine-standard+
  '((:hourly 0.095 :initial 0.0)
    (:hourly 0.38 :initial 0.0)))
(defparameter +bbop-machine-reserved+
  '((:hourly 0.04 :initial 227.5)
    (:hourly 0.16 :initial 910.0)))
(defparameter +bbop-machine-mini-standard+
  '((:hourly 0.020 :initial 0.0)
    (:hourly 0.095 :initial 0.0)))
(defparameter +bbop-machine-mini-reserved+
  '((:hourly 0.01 :initial 54.0)
    (:hourly 0.04 :initial 227.5)))
(defparameter +bbop-machine-mini-reserved-only+
  '((:hourly 0.01 :initial 54.0)))

(defun total-calc (machine-list)
  (+ (bbop-ec2-cpu-calc machine-list)
     (bbop-ec2-ebs-data-calc (length machine-list))
     (bbop-s3-calc)))
