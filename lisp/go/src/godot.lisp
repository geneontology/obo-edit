;;;;
;;;; GO Data and Object Table
;;;;
;;;; NOTE: currently standalone--not part of the go package.
;;;;

;;(require :toolkit)
(require :toolkit)
(require :parenscript)
(require :hunchentoot)
(require :cl-who)

(defpackage :godot
  (:use :cl
	:toolkit-conv
	;; :hunchentoot ; explicit calls to hunchentoot while I learn it
	:parenscript))
(in-package :godot)

;; Our global server.
(defvar *server* nil)
(defvar *location* "/srv/www/hunchentoot/")


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
  (let ((ubase "http://localhost/amigo/js/com/"))
    (cl-who:with-html-output-to-string (strm)
      (:html (:head

	      ;; CSS.
	      (:style ".draggable{background:pink;width:100px;height:100px;border-style:solid;border-width:1px;border-color:red;position:absolute;top:200px;left:200px;}")
	      ;; JS includes.
	      (:script :type "text/javascript"
		       :src (ccat ubase "jquery-1.3.2.min.js"))
	      (:script :type "text/javascript"
		       :src (ccat ubase "jquery-ui-1.7.1.custom.min.js"))
	      (:script :type "text/javascript"
		       :src "http://localhost:4242/godot.js"))

	     (:body
	      (:h2 "GODOT")
	      (:div :id "godot")
	          
	      (:div :id "location" "current div location")
	      (:div :id "snap" "snap to")
	      (:div :id "new-term" (:a :href "#" "new term"))
	      (:div :id "new-association" (:a :href "#" "new association"))
	      (:div :id "new-gp" (:a :href "#" "new gene product"))

	      (:div :id "zoo")
	      
	      )))))


;;
(setf parenscript:*js-string-delimiter* #\")

;;;
;;; Add some macros to the environment.
;;;

;; Generates itself into the document.
(defpsmacro entity-generator (type)
  
  (let* ((s-type (string-downcase (symbol-name type)))
	 (f-type (ccat "*" s-type "-entity")))

  `(defun ,(intern f-type) ()

     (var my-id ((slot-value gen-uid 'uid)))

     ;; Create new div.
     (var new-div ((slot-value document 'create-element) "div")) 
     ((slot-value new-div 'set-attribute) "id" my-id)
     ((slot-value new-div 'set-attribute) "class" (+ "draggable" " " ,s-type))
     (setf (slot-value new-div 'inner-h-t-m-l) (+ ,s-type " " my-id))

     ;; Set internal properties.
     (setf (slot-value this 'id) my-id)
     (setf (slot-value this 'div) new-div)
     (setf (slot-value this 'type) ,s-type)

     ;; Add to object-list.
     (setf (slot-value *object-list* my-id) this)

     ;; Add to "zoo".
     (var the-zoo ((slot-value document 'get-element-by-id) "zoo"))
     ((slot-value the-zoo 'append-child) new-div)

     ((slot-value (j-query new-div) 'draggable)
      (create :start (lambda (e ui)
		       ;;(print-div-location e)
		       )
	      :drag (lambda (e ui)
		      ;;(print-div-location e)
		      )
	      :stop (lambda (e ui)
		      (let ((eid (slot-value (slot-value e 'target) 'id)))
			(snap-to eid))))))))


;;(defun godot-js (foo)
(defun godot-js ()
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
   
;;;
;;; Object generation for table.
;;;

   (defun *generator ()
     (var id 0)
     (setf (slot-value this 'uid)
	   (lambda ()
	     (setf id (+ id 1))
	     (return (+ "_mangled_" id)))))

   (defvar gen-uid (new (*generator)))

   ;; Generates itself into the document.
   (entity-generator term)
   (entity-generator association)
   (entity-generator gp)
;;    (defun *term-entity ()

;;      (var my-id ((slot-value gen-uid 'uid)))

;;      ;; Create new div.
;;      (var new-div ((slot-value document 'create-element) "div")) 
;;      ((slot-value new-div 'set-attribute) "id" my-id)
;;      ((slot-value new-div 'set-attribute) "class" (+ "draggable" " " "term"))
;;      (setf (slot-value new-div 'inner-h-t-m-l) (+ "term" " " my-id))

;;      ;; Set internal properties.
;;      (setf (slot-value this 'id) my-id)
;;      (setf (slot-value this 'div) new-div)
;;      (setf (slot-value this 'type) "term")

;;      ;; Add to object-list.
;;      (setf (slot-value *object-list* my-id) this)

;;      ;; Add to "zoo".
;;      (var the-zoo ((slot-value document 'get-element-by-id) "zoo"))
;;      ((slot-value the-zoo 'append-child) new-div)

;;      ((slot-value (j-query new-div) 'draggable)
;;       (create :start (lambda (e ui)
;; 		       ;;(print-div-location e)
;; 		       )
;; 	      :drag (lambda (e ui)
;; 		      ;;(print-div-location e)
;; 		      )
;; 	      :stop (lambda (e ui)
;; 		      (let ((eid (slot-value (slot-value e 'target) 'id)))
;; 			(snap-to eid))))))

;;;
;;; UI handling.
;;; TODO: UI for filters to stack.
;;;

   ;; "m" is the snap limit.
   (defun calc-snap (x)
     (let* ((m 100) (r (% x m)))
       (if (> r (/ m 2))
 	   (return (+ m (- x r)))
 	   (return (- x r)))))

   ;;
   (defun snap-to (id)
     (let* ((left (div-pos-left id))
	    (top (div-pos-top id))
	    (new-left (calc-snap left id))
	    (new-top (calc-snap top id)))

       (print-to-div "location" (+ "div: " id ": " left ", " top))
       (print-to-div "snap" (+ "snap-to: " id ": " new-left ", " new-top))

       ((slot-value (j-query (+ "#" id)) 'animate)
	(create ;;:position "relative"
	 :left (+ new-left "px")
	 :top (+ new-top "px"))
	"fast")))

   ;;
   (defun print-to-div (id html)
     ((slot-value (j-query (+ "#" id)) 'html) html))

   ;; TODO:
   ;; (defpsmacro div-pos (id coord)
   ;;   '(return (pos (slot-value (j-query (+ "#" id)) 'position)) ,coord
   ;;   (left (slot-value pos 'left))
   (defun div-pos-left (id)
     (let ((pos ((slot-value (j-query (+ "#" id)) 'position))))
       (return (slot-value pos 'left))))
   (defun div-pos-top (id)
     (let ((pos ((slot-value (j-query (+ "#" id)) 'position))))
       (return (slot-value pos 'top)))) 


   ;; Strange to think that this is onReady...
   ((slot-value (j-query) 'ready)
    (lambda ()

      ;; Generate new terms on click.
      ((slot-value (j-query "#new-term") 'click)
       (lambda (e)
	 (new (*term-entity))))

      ;; Generate new assocs on click.
      ((slot-value (j-query "#new-association") 'click)
       (lambda (e)
	 (new (*association-entity))))

      ;; Generate new assocs on click.
      ((slot-value (j-query "#new-gp") 'click)
       (lambda (e)
	 (new (*gp-entity))))

      ;; (dump "so...here we are...")
   
;;;
;;; Object table: provides the graph model to UI pieces for
;;; communication back to the server for calculation (the server will
;;; just be taking a JSON graph representation).
;;;
   
   ;; TODO: Get all information about draggable objects positions.
   ;;   
   ;; DO over .draggable and collect position into graph.

   ;; TODO: use graph positions and r/p table to determine the graph
   ;; to send back
   
   ))))

;;;
;;; PS macro work and advanced PS features.
;;;

;; Nothing special.
(defun t1 ()
  (ps (alert)))

;; Defined PS macro.
(defpsmacro 1- (form)
  `(- ,form 1))

;; Use PS macro.
(defun t2 ()
  (ps (alert (1- 5))))

;; Grab lisp variable.
(defun t3 (x)
  (ps (alert (1- (lisp x)))))
