;;;;
;;;; Graphing for the GO.
;;;;
;;;; Look at tests to get usage...
;;;;

(clc:clc-require :metatilities-base)
(clc:clc-require :cl-containers)
(clc:clc-require :cl-graph)
(defpackage :go-graph
  (:use :cl
	:cl-graph)
  (:export :hello))
(in-package :go-graph)


;;
(defun hello ()
  "Hello."
  nil)

;;;
;;; The inspect function drops you into a graph REPL apparently...
;;;

;; (setf graph (cl-containers::make-container 'graph-container :default-edge-type :directed))
;; (add-vertex graph "GO:1")
;; (add-vertex graph "GO:2")
;; (add-edge-between-vertexes graph (first (vertexes graph)) (second (vertexes graph)))
;; (search-for-vertex graph "GO:2" :key 'element :error-if-not-found? nil)
;; (setf (color (first (edges graph))) 'is-a)
;; (describe (first (edges graph)))
;; (graph-roots graph)
