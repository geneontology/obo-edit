;;;;
;;;; Assuming that you have bbop, mechanize, and tanuki in your asdf
;;;; path and are using quicklisp, the following should get the right
;;;; libraries in.
;;;;

(let ((libs (list "postmodern" "drakma" "trivial-timeout"
		  "closure-html" "local-time" "fiveam")))
  (dolist (l libs)
    (ql:quickload l)))
