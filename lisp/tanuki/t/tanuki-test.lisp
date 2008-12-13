;;;;
;;;; Usage:
;;;; (load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki-decide.lisp")
;;;; (load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki-test.lisp")
;;;; (load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki-decide.lisp")
;;(load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki-html.lisp")
;;(load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki-db.lisp")
;;(load "/home/sjcarbon/local/src/svn/bdgp/amigo2/lib/lisp/tanuki.lisp")

(clc:clc-require :tanuki-decide)
(clc:clc-require :tanuki-html)
(clc:clc-require :tanuki)
(clc:clc-require :fiveam)
(defpackage :tanuki-test (:use :cl
			       :tanuki-html
			       :tanuki-decide
			       :tanuki
			       :fiveam))
;;:5am))
(in-package :tanuki-test)


(defun run-tests()
  "General top-level test runner."
  (format t "Running tanuki-decide tests:~&")
  (run! 'decide-tests)
  (format t "Running tanuki-html tests:~&")
  (run! 'html-tests))

;;(def-suite tanuki-suite :description "tanuki-decide test suite")
;;(in-suite tanuki-suite)

;;
(test decide-tests
      "A selection of simple first-pass tests for tanuki-decide."

      ;; levenshtein-distance
      (is (zerop (levenshtein-distance "kitten" "kitten")) "Equals")
      (is (= (levenshtein-distance "kitten" "") 6) "Against nothing")
      (is (= (levenshtein-distance "kitten" "sitting") 3) "Similar")

      ;; decide
      (is (string= (decide '("yyy" "aaa" "pdq" "xxy")
			   '("xxx" "xxy" "aab" "yyx" "bab")) 
		   "pdq") "Should find \"pdq\"")

      ;; TODO: always more cases...
      )

(test html-tests
      "A selection of simple first-pass tests for tanuki-html."

      ;;
      ;; TODO: It would be nice to have an web server that spins up
      ;; here so we can work off of it. Maybe after the web front-end
      ;; is done?
      ;;

      ;; ok-code
      (is (eq (ok-code 200) t)
	  "200 should be OK")
      (is (eq (ok-code 399) t)
	  "388 should be OK")
      (is (eq (ok-code 400) nil)
	  "400 should not be OK")
      (is (eq (ok-code 401) nil)
	  "401 should not be OK")

      ;; fetch-doc
      (is (eq (fetch-doc "www.google") nil)
	  "Shouldn't be able to resolve \"www.google\"")
      (is (eq (fetch-doc "http://www.google") nil)
	  "Shouldn't be able to resolve \"http://www.google\"")      
      ;(is (eq (fetch-doc "http://www.google") nil)
;	  "Shouldn't be able to resolve \"http://www.google\"")
      

;;       ;; get-base-uri/set-base-uri
;;       (signals
;;        (error "Bad URL: not http")
;;        (set-base-uri "file://foo/bar"))
;;       (signals
;;        (error "Bad URL: no URL")
;;        (set-base-uri ""))
;;       (signals
;;        (error "Bad URL: no host")
;;        (set-base-uri "http:///bar/foo.htm"))
;;       (finishes (set-base-uri "http://www.foo.com/cgi")
;; 		"Couldn't set base URI")

      ;;; fetch-doc
      (let ((doc (fetch-doc "http://www.google.com")))
	(is (and (stringp doc) (> (length doc) 0))
	    "Couldn't fetch live doc (google)."))
      (let ((doc (fetch-doc "http://www.gnu.org")))
	(is (and (stringp doc) (> (length doc) 0))
	    "Couldn't fetch live doc (gnu)."))
      ;;(signals
      ;; (error "Wuzzup?")
       ;;(fetch-doc "http://www.iamnotarealdomainofanykind.com"))

      ;; internal-p
      (let ((base "http://localhost/my.cgi"))
	(is (eq (internal-p base base) t)
	    "Same should be internal")
	(is (eq (internal-p "/a.html" base) nil)
	    "Pieces should not be internal")
	(is (eq (internal-p "http://localhost/my.cgi/foo" base) t)
	    "Relative shift should be internal")
	(is (eq (internal-p "http://localhost" base) t)
	    "Domain should be internal")
	(is (eq (internal-p "http://localhost:9000" base) nil)
	    "Different port should not be internal")
	(is (eq (internal-p "http://localhost:80" base) t)
	    "Same port should be internal")
	(is (eq (internal-p "http://foo.localhost/my.cgi" base) nil)
	    "Different subdomain should not be internal")
	(is (eq (internal-p "http://externalhost/my.cgi" base) nil)
	    "Different domain should not be internal"))

      ;; TODO: Add more and more tests...
      )
