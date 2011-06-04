;;;;
;;;; http://aws.amazon.com/s3/#pricing
;;;; http://aws.amazon.com/ec2/#pricing
;;;;
;;;; Amazon EC2 and EBS charges:
;;;;
;;;; Standard prices.
;;;; | name          |  mem | comp         | price |
;;;; |---------------+------+--------------+-------|
;;;; | micro         | .613 | 2*           | 0.025 |
;;;; | std small     |  1.7 | 1            | 0.095 |
;;;; | hi-cpu small  |  1.7 | 5 (2*2.5)    |  0.19 |
;;;; | std large     |  7.5 | 4 (2*2)      |  0.38 |
;;;; | hi-mem small  | 17.2 | 6.5 (2*3.25) |  0.57 |
;;;; | std xlarge    |   15 | 8 (4*2)      |  0.76 |
;;;; | hi-cpu large  |  7   | 20 (8*2.5)   |  0.76 |
;;;; | hi-mem large  | 34.2 | 13 (4*3.25)  |  1.14 |
;;;; | hi-mem xlarge | 68.4 | 26 (8*3.25)  |  2.28 |
;;;;
;;;; Lower prices with per-buy (reserved). 
;;;; | name          |  mem | comp         | price | year cover |
;;;; |---------------+------+--------------+-------+------------|
;;;; | micro         | .613 | 2*           |  0.01 |         54 |
;;;; | std small     |  1.7 | 1            |  0.04 |     227.50 |
;;;; | hi-cpu small  |  1.7 | 5 (2*2.5)    |  0.08 |        455 |
;;;; | std large     |  7.5 | 4 (2*2)      |  0.16 |        910 |
;;;; | hi-mem small  | 17.2 | 6.5 (2*3.25) |  0.24 |       1325 |
;;;; | std xlarge    |   15 | 8 (4*2)      |  0.32 |       1820 |
;;;; | hi-cpu large  |    7 | 20 (8*2.5)   |  0.32 |       1820 |
;;;; | hi-mem large  | 34.2 | 13 (4*3.25)  |  0.48 |       2650 |
;;;; | hi-mem xlarge | 68.4 | 26 (8*3.25)  |  0.96 |       5300 |
;;;;
;;;;   3) 5GB images
;;;;   4) restarted once a week (image i/o)
;;;;   5) every month, they move their image size in and out as standard traffic
;;;;   6) with a 10GB EBS each for backing and database. 
;;;;   7) every month, they move their store size in and out as standard traffic
;;;;   8) for EBS, i/o request is 100 per second (continuous and ridiculous)
;;;;

;;;
;;; Invariables.
;;;

;; Useful definitions.
(defvar *days-per-year* 365.242199)
(defvar *days-per-month* (/ *days-per-year* 12.0))
(defvar *hours-per-month* (* 1.0 24.0 *days-per-month*))
(defvar *seconds-per-month* (* 1.0 60.0 60.0 *hours-per-month*))

;; Amazon global charges.
(defvar *aws-gb-xfer-out-charge* 0.150)
(defvar *aws-gb-xfer-in-charge* 0.100)

;; Amazon S3 charges.
(defvar *aws-s3-gb-store-charge* 0.165)
(defvar *aws-s3-1000-op-charge* 0.011)

;; Amazon EBS numbers charges.
(defvar *aws-ebs-gb-store-charge* 0.11)
(defvar *aws-ebs-1000000-io-charge* 0.11)

;; LBL variables.
(defparameter +lbl-contract-months+ 12.0 "LBL contract length in months.")

;; BBOP general.
(defparameter +bbop-monthly-churn-rate+ 0.5)
(defparameter +bbop-monthly-restarts+ 4.0)

;; BBOP S3 variables.
(defparameter +bbop-s3-stored-data-gb+ 100.0)
(defparameter +bbop-s3-ops-per-day+ 1000.0)

;; BBOP EC2.
(defparameter +bbop-ec2-image-size-gb+ 5.0)

;; BBOP EBS.
(defparameter +bbop-ebs-store-gb+ 10.0)
;; NOTE: Interesting variable to play with; according to amazon, a
;; medium sized website might be around 100.
(defparameter +bbop-ebs-io-tps+ 10.0)

;; BBOP machine configurations.
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

;;;
;;; S3 calcualtions.
;;;

(defun bbop-s3-ops-monthly ()
  "Calculate charge of ops for storage using ops estimate."
  (* (/ (* +bbop-s3-ops-per-day+ 31.0) 1000.0) *aws-s3-1000-op-charge*))

(defun bbop-s3-store-monthly ()
  "Calculate s3 storage cost; stored a data is always max."
  (* +bbop-s3-stored-data-gb+ *aws-s3-gb-store-charge*))

(defun bbop-s3-xfer-monthly ()
  "If half is moved in every month and half moved out, churn is 0.5."
  (let ((churned-data (* +bbop-s3-stored-data-gb+ +bbop-monthly-churn-rate+)))
    (* churned-data (+ *aws-gb-xfer-out-charge* *aws-gb-xfer-in-charge*))))

;; Calc.
(defun bbop-s3-total ()
  (* +lbl-contract-months+
     (+ (bbop-s3-ops-monthly)
	(bbop-s3-store-monthly)
	(bbop-s3-xfer-monthly))))

;;;
;;; EC2 and EBS.
;;;

(defun bbop-ebs-data-monthly ()
  (+ (* +bbop-ebs-store-gb+ *aws-ebs-gb-store-charge*)
     (* +bbop-monthly-churn-rate+ +bbop-ebs-store-gb+
	(+ *aws-gb-xfer-in-charge* *aws-gb-xfer-out-charge*))))

(defun bbop-ec2-restart-monthly ()
  (* +bbop-monthly-restarts+ +bbop-ec2-image-size-gb+ *aws-gb-xfer-in-charge*))

(defun bbop-ec2-ops-monthly ()  
  (* (/ (* *seconds-per-month* +bbop-ebs-io-tps+) 1000000.0)
     *aws-ebs-1000000-io-charge*))

;; Calculate io and storage (data) costs.
(defun bbop-ec2-ebs-data-total (num-machines)
  (* num-machines +lbl-contract-months+ ;; machines over contract
     (+
      (bbop-ebs-data-monthly)
      (bbop-ec2-weekly-restart-monthly)
      (bbop-ec2-ops-monthly))))

(defun bbop-ec2-cpu-total (machine-list)
  "..."
  (cond 
    ((atom machine-list) 0.0)
    (t
     (+ (let ((machine-entry (car machine-list)))
	  (+ (* +lbl-contract-months+ *hours-per-month*
		(getf machine-entry :hourly))
	     (getf machine-entry :initial)))
	(bbop-ec2-cpu-monthly (cdr machine-list))))))

;;;
;;; 
;;;

(defun overall-calc (machine-list)
  (+ (bbop-s3-total)
     (bbop-ec2-ebs-data-total (length machine-list))
     (bbop-ec2-cpu-total machine-list)))

;; Calculate what backups might look like.  Note, only looking at
;; machine and storage, no i/o (assuming it would be low as only
;; initial and diff).
(defun bbop-ec2-backup-calc ()
  (let ((+bbop-ec2-image-size-gb+ 10.0)
	(+bbop-ebs-store-gb+ 1000.0)
	(+bbop-ebs-io-tps+ 100.0)
	(+bbop-monthly-churn-rate+ 0.25))
    (+ (bbop-ec2-cpu-total +bbop-machine-mini-reserved-only+)
       (* +lbl-contract-months+
	  (+ (bbop-ec2-ops-monthly)
	     (bbop-ebs-data-monthly))))))
(defun bbop-s3-backup-calc ()
  (let ((+bbop-s3-stored-data-gb+ 1000.0)
	(+bbop-monthly-churn-rate+ 0.25)
	(+bbop-s3-ops-per-day+ 1000.0))
    (bbop-s3-total)))
