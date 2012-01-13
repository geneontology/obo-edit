;;;;
;;;; GO Data and Object Table
;;;;
;;;; NOTE: currently standalone--not part of the go package.
;;;;

(require :day-to-day)
(require :cl-who)
(require :parenscript)
(require :hunchentoot)
;;(require :gold)
(defpackage :godot
  (:use :cl
	;; :toolkit-string
	;; :hunchentoot ; explicit calls to hunchentoot while I learn it
	:parenscript))
(in-package :godot)

;; Our global server.
(defvar *server* nil)
(defvar *location* "/srv/www/hunchentoot")

;; HT logs.
(setf HUNCHENTOOT::*MESSAGE-LOG-PATHNAME* (d2d:ccat *location* "/comb.log"))
;; (setf HUNCHENTOOT::*ACCESS-LOG-PATHNAME* *location*)
(setf HUNCHENTOOT::*SHOW-LISP-ERRORS-P* t)

;;;
;;; Server control.
;;; 

;; ;; Hunchentoot settings. NOTE: no longer in newest version
;; (setq hunchentoot:*catch-errors-p* nil)
;; (setq hunchentoot:*log-lisp-backtraces-p* t)

;; 
(defun start ()
  ;; TODO: still working on this...
  ;;   (setq hunchentoot:*dispatch-table*
  ;; 	(nconc
  ;; 	 ;; 	 (list 'dispatch-easy-handlers
  ;; 	 ;; 	       (create-static-file-dispatcher-and-handler
  ;; 	 ;; 		"/hunchentoot/test/favicon.ico"
  ;; 	 ;; 		(make-pathname :name "favicon" :type "ico" :version nil
  ;; 	 ;; 			       :defaults *this-file*))
  ;; 	 ;; 		(make-pathname :name nil :type nil :version nil
  ;; 	 ;; 			       :defaults *this-file*)
  ;; 	 ;; 		"text/plain"))
  ;; 	 (mapcar (lambda (args)
  ;; 		   (apply 'create-prefix-dispatcher args))
  ;; 		 '(("/godot.js" godot-js)))
  ;; 	 (list 'default-dispatcher)))
  
  (hunchentoot:define-easy-handler (what-is-this-1 :uri "/godot.html") ()
    (setf (hunchentoot:content-type*) "text/html")
    (godot-html-base))

  ;;   (hunchentoot:define-easy-handler (what-is-this-2 :uri "/godot.js") (foo)
  ;;     (setf (hunchentoot:content-type*) "application/javascript")
  ;;     (godot-js foo))
  (hunchentoot:define-easy-handler (what-is-this-2 :uri "/godot.js") ()
    (setf (hunchentoot:content-type*) "application/javascript")
    (godot-js))

  (setf *server* (make-instance 'hunchentoot:acceptor :port 4242))
  (hunchentoot:start *server*))

;;   (setq hunchentoot:*dispatch-table*
;; 	(nconc
;; 	 (mapcar (lambda (args)
;; 		   (apply #'hunchentoot:create-prefix-dispatcher args))
;; 		 '(("/static/" serve-static)
;; 		   ("/what-color" what-color?)
;; 		   ("/main" main)))
;; 	 (list #'hunchentoot:default-dispatcher)))
;;   (setf *server* (hunchentoot:start-server :port 4242)))

;; 
(defun stop ()
  (hunchentoot:stop *server*))

;;;
;;; Page setup.
;;;


(defun godot-html-base ()
  (let ((ubase "http://localhost/amigo2/javascript/"))
    (cl-who:with-html-output-to-string (strm)
      (:html
       (:head
	;; CSS.
	;;(:style ".draggable{background:pink;width:100px;height:100px;border-style:solid;border-width:1px;border-color:red;position:absolute;top:200px;left:200px;}")
	;; JS includes.
	(:script :type "text/javascript"
		 :src (d2d:ccat ubase "com/jquery-1.5.1.min.js"))
	(:script :type "text/javascript"
		 :src (d2d:ccat ubase "com/jquery-ui-1.8.13.custom.min.js"))
	(:script :type "text/javascript"
		 :src (d2d:ccat ubase "com/raphael-min.js"))
	(:script :type "text/javascript"
		 :src "http://localhost:4242/godot.js"))

       (:body
	(:h2 "GODOT")
	(:div :id "zoo")	      
	(:div :id "godot")
	
	;;(:div :id "location" "current div location")
	;;(:div :id "snap" "snap to")
	;;(:div :id "new-term" (:a :href "#" "new term"))
	;;(:div :id "new-association" (:a :href "#" "new association"))
	;;(:div :id "new-gp" (:a :href "#" "new gene product"))
	
	)))))

;;
(setf parenscript:*js-string-delimiter* #\")

;;;
;;; Add some macros to the environment.
;;;

;; ;; Generates itself into the document.
;; (defpsmacro entity-generator (type)
  
;;   (let* ((s-type (string-downcase (symbol-name type)))
;; 	 (f-type (d2d:ccat "*" s-type "-entity")))

;;   `(defun ,(intern f-type) ()

;;      (var my-id ((getprop gen-uid 'uid)))

;;      ;; Create new div.
;;      (var new-div ((getprop document 'create-element) "div")) 
;;      ((getprop new-div 'set-attribute) "id" my-id)
;;      ((getprop new-div 'set-attribute) "class" (+ "draggable" " " ,s-type))
;;      (setf (getprop new-div 'inner-h-t-m-l) (+ ,s-type " " my-id))

;;      ;; Set internal properties.
;;      (setf (getprop this 'id) my-id)
;;      (setf (getprop this 'div) new-div)
;;      (setf (getprop this 'type) ,s-type)

;;      ;; Add to object-list.
;;      (setf (getprop *object-list* my-id) this)

;;      ;; Add to "zoo".
;;      (var the-zoo ((getprop document 'get-element-by-id) "zoo"))
;;      ((getprop the-zoo 'append-child) new-div)

;;      ((getprop (j-query new-div) 'draggable)
;;       (create :start (lambda (e ui)
;; 		       ;;(print-div-location e)
;; 		       )
;; 	      :drag (lambda (e ui)
;; 		      ;;(print-div-location e)
;; 		      )
;; 	      :stop (lambda (e ui)
;; 		      (let ((eid (getprop (getprop e 'target) 'id)))
;; 			(snap-to eid))))))))


;;(defun godot-js (foo)
(defun godot-js ()
  ;; Get stuff out of database.
  (let ((foo nil))
    (gold:connect)
    (setf foo (gold:list-tables)
    (gold:disconnect)

  ;; Produce JS.
  (ps
    
;;;
;;; Global bookkeeping objects.
;;;
    
    ;; TODO: Define relations/properties table for GO db objects.
    (defvar *object-list* (create))
    (defvar *object-graph* (create))
    (defvar *object-properties*
      (create :term (create :okay (create :association t))
	      :association (create :okay (create :term t :gene_product t))
	      :gene_product (create :okay (create :association t))))

)))
    
;;;
;;; Object generation for table.
;;;
    
    ;; (defun *generator ()
    ;;   (var id 0)
    ;;   (setf (getprop this 'uid)
    ;; 	    (lambda ()
    ;; 	      (setf id (+ id 1))
    ;; 	      (return (+ "_mangled_" id)))))
    
    ;; (defvar gen-uid (new (*generator)))

    ;; Generates itself into the document.
    ;; (entity-generator term)
    ;; (entity-generator association)
    ;; (entity-generator gp)
    
;;;
;;; UI handling.
;;; TODO: UI for filters to stack.
;;;
    
;;     ;; "m" is the snap limit.
;;     (defun calc-snap (x)
;;       (let* ((m 100) (r (rem x m)))
;; 	(if (> r (/ m 2))
;; 	    (return (+ m (- x r)))
;; 	    (return (- x r)))))

;;     ;;
;;     (defun snap-to (id)
;;       (let* ((left (div-pos-left id))
;; 	     (top (div-pos-top id))
;; 	     (new-left (calc-snap left id))
;; 	     (new-top (calc-snap top id)))
	
;; 	(print-to-div "location" (+ "div: " id ": " left ", " top))
;; 	(print-to-div "snap" (+ "snap-to: " id ": " new-left ", " new-top))
	
;; 	((getprop (j-query (+ "#" id)) 'animate)
;; 	 (create ;;:position "relative"
;; 	  :left (+ new-left "px")
;; 	  :top (+ new-top "px"))
;; 	 "fast")))

;;     ;;
;;     (defun print-to-div (id html)
;;       ((getprop (j-query (+ "#" id)) 'html) html))

;;     ;; TODO:
;;     ;; (defpsmacro div-pos (id coord)
;;     ;;   '(return (pos (getprop (j-query (+ "#" id)) 'position)) ,coord
;;     ;;   (left (getprop pos 'left))
;;     (defun div-pos-left (id)
;;       (let ((pos ((getprop (j-query (+ "#" id)) 'position))))
;; 	(return (getprop pos 'left))))
;;     (defun div-pos-top (id)
;;       (let ((pos ((getprop (j-query (+ "#" id)) 'position))))
;; 	(return (getprop pos 'top)))) 

    
;;     ;; Strange to think that this is onReady...
;;     ((getprop (j-query) 'ready)
;;      (lambda ()

;;        ;; Generate new terms on click.
;;        ((getprop (j-query "#new-term") 'click)
;; 	(lambda (e)
;; 	  (new (*term-entity))))

;;        ;; Generate new assocs on click.
;;        ((getprop (j-query "#new-association") 'click)
;; 	(lambda (e)
;; 	  (new (*association-entity))))
       
;;        ;; Generate new assocs on click.
;;        ((getprop (j-query "#new-gp") 'click)
;; 	(lambda (e)
;; 	  (new (*gp-entity))))
       
;;        ;; (dump "so...here we are...")
       
;; ;;;
;; ;;; Object table: provides the graph model to UI pieces for
;; ;;; communication back to the server for calculation (the server will
;; ;;; just be taking a JSON graph representation).
;; ;;;
   
;;        ;; TODO: Get all information about draggable objects positions.
;;        ;;   
;;        ;; DO over .draggable and collect position into graph.
       
;;        ;; TODO: use graph positions and r/p table to determine the graph
;;        ;; to send back
       
;;       ))))

;;;
;;; PS macro work and advanced PS features.
;;;

;; Nothing special.
(defun t1 ()
  (ps (alert)))

;; ;; Defined PS macro.
;; (defpsmacro 1- (form)
;;   `(- ,form 1))

;; Use PS macro.
(defun t2 ()
  (ps (alert (1- 5))))

;; Grab lisp variable.
(defun t3 (x)
  (ps (alert (1- (lisp x)))))
