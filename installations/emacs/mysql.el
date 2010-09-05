;;;;
;;;; MySQL shell with completion through comint and dictionary pulled
;;;; from mysqlshow (through shell).
;;;;
;;;; To run in this way, you'd have to define *full-path-to-here* to
;;;; this directory. 
;;;;
;;;; Once loaded, to invoke: M-x run-mysql
;;;;


(require 'sql)

;;
(defun run-mysql ()
  (interactive)
  (sql-mysql)
  ;; NOTE: Change the below path for your environment.
  (let ((library-to-load (concat *full-path-to-here* "my2sql.el")))
    (when (file-exists-p library-to-load)
      (load-file library-to-load)
      (my2sql-make-dictionary)
      (define-key sql-interactive-mode-map "\t" 'my2sql-complete))))
