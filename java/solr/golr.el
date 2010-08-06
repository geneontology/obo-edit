;;;; #!/usr/bin/emacs --script
;;;; 
;;;; Commands for redeploying Solr with new files from within Emacs.
;;;;
;;;; Before use:
;;;;  M-x load-library ~/local/src/svn/geneontology/java/solr/golr.el
;;;;

(require 'cl)
(require 'url)

;;;
;;; Changable variables.
;;; BUG/TODO: Should use emacs config/prefs system.
;;;

(defconst solr-location
  "http://accordion.lbl.gov:8080/solr/"
  "The base URL of your Solr installation.")

(defconst dir-location
  "~/local/src/svn/geneontology/java/solr/"
  "The location of this file on your file system inside its SVN repository.")

(defconst golr-xfers
  '(("apache" ("default") "/etc/apache2/sites-available/")
    ("jetty" ("jetty.conf" "jetty-rewrite.xml") "/etc/jetty/")
    ("jetty" ("no_access.html") "/var/lib/jetty/webapps/root/")
    ("solr" ("go-data-config.xml" "schema.xml" "solrconfig.xml") "/etc/solr/conf/"))
  "File transfers to make when \"installing\" and before restarting.")

(defconst golr-cmnds
  '("/etc/init.d/jetty stop"
    "/etc/init.d/jetty start"
    "/etc/init.d/apache2 restart")
  "Commands to run (as sudo) to restart all of the necessary system services.")

;;;
;;; Support functions.
;;;

(defun golr-sudo-prep ()
  "Warm-up sudo for other commands."
  (shell-command (concat "echo \""
			 (read-passwd "Password: ")
			 "\" | sudo -S whoami")))

(defun golr-copy-file (src dest)
  "Warm-up sudo for other commands."
  (shell-command (concat "sudo cp " src " " dest)))

(defun golr-file-rollout ()
  "Moves the SVN files into place. Uses golr-xfers as struct."
  (dolist (x golr-xfers)
    (let ((idir (car x))
	  (ilist (cadr x))
	  (itarget (car (last x))))
      (dolist (i ilist)
	(let ((src (concat idir "/" i))
	      (dest (concat itarget i)))
	  (princ (concat src " to " dest "\n"))
	  ;;(copy-file src dest)
	  (golr-copy-file src dest))))))

(defun golr-services-restart ()
  "Restart services to get Solr/Jetty/Apache back into a testing state."
  (dolist (c golr-cmnds)
    (shell-command (concat "sudo " c))))

;;;
;;; Interactive commands.
;;;

(defun golr-restart ()
  "Deploy all files and restart all services."
  (interactive)
  (cd dir-location)
  (golr-sudo-prep)
  (golr-file-rollout)
  (golr-services-restart)
  (princ "golr-restart completed--solr restarting!"))

(defun golr-update ()
  "Starts the update from "
  (interactive)
  (url-retrieve-synchronously
   (concat solr-location "dataimport?command=full-import"))
  (princ "golr-update completed--update has started!"))
