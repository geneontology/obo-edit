#!/usr/bin/emacs --script
;;;; 
;;;; Commands for redeploying a package manager distributed Solr/Jetty
;;;; instance with GO data. Can be used from the command line or from
;;;; within Emacs after loading this file.
;;;;
;;;; Usage (cli):
;;;;    sudo ./golr.el
;;;;
;;;; Usage (emacs):
;;;;    M-x load-library <path to this file>
;;;;    M-x golr-
;;;;
;;;; WARNING: These commands use sudo and can affect the filesystem
;;;; and running processes.
;;;;
;;;; BUG: figure out why sudo is wonky and merge golr-no-sudo-restart
;;;; and golr-restart.
;;;;
;;;; BUG: why the double output on my terminal?
;;;;

(require 'cl)
(require 'url)

;;;
;;; Customizable variables.
;;;

(defgroup golr nil
  "GO in Solr Emacs group."
  :prefix "golr-"
  :group 'emacs)

;; (defgroup golr-solr nil
;;   "Solr and GO Emacs group."
;;   :prefix "golr-solr-"
;;   :group 'golr)

;; Another example might be: "http://accordion.lbl.gov:8080/solr/"
(defcustom golr-solr-url "http://localhost:8080/solr/"
  "URL of the Solr installation in question."
  :type 'string
  :group 'golr)

(defcustom golr-location "~/local/src/svn/geneontology/java/gold/solr/"
  "The location of this file on your file system inside its SVN
repository."
  :type 'string
  :group 'golr)

(defcustom golr-transfer-schema
  '(("apache" ("golr") "/etc/apache2/sites-available/")
    ("jetty" ("jetty.conf" "jetty-rewrite.xml" "jetty.xml") "/etc/jetty/")
    ("jetty" ("no_access.html") "/var/lib/jetty/webapps/root/")
    ("conf" ("go-data-config.xml" "schema.xml" "solrconfig.xml") "/etc/solr/conf/"))
  "File transfers to make when \"installing\" and before
  restarting. For each sub-list, the first item is the
  sub-directory in golr-location, the second item is a list of
  files in that directory to forcably copy, the third item is the
  target directory for the copying."
  :type 'sexp
  :group 'golr)

(defcustom golr-command-schema
  '("/etc/init.d/jetty stop"
    "/etc/init.d/jetty start"
    "/etc/init.d/apache2 restart")
  "Commands to run (as sudo) to restart all of the necessary
system services. It is an ordered list of strings."
  :type 'sexp
  :group 'golr)

;;;
;;; Support functions.
;;;

(defun golr-shell-command (cmnd)
  "Standard shell command with printing."
  (princ (concat "[Golr] " cmnd "\n"))
  (shell-command cmnd))

(defun golr-sudo-prep ()
  "Warm-up sudo for other commands."
  (princ "[Golr] Getting sudo password cached...")
  (shell-command (concat "echo \""
			 (read-passwd "Password: ")
			 "\" | sudo -S whoami")))

(defun golr-copy-file (src dest)
  "Warm-up sudo for other commands."
  (golr-shell-command (concat "sudo cp " src " " dest)))

(defun golr-file-rollout ()
  "Moves the SVN files into place. Uses golr-transfer-schema as struct."
  (dolist (x golr-transfer-schema)
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
  (dolist (c golr-command-schema)
    (golr-shell-command (concat "sudo " c))))

;;;
;;; Interactive commands.
;;;

(defun golr-no-sudo-restart ()
  "Deploy all files and restart all services as if had sudo. If running interactively in emacs, you probably want golr-restart instead."
  (interactive)
  (cd golr-location)
  (golr-file-rollout)
  (golr-services-restart)
  (princ "golr-restart completed--Solr restarting!\n"))

(defun golr-restart ()
  "Deploy all files and restart all services."
  (interactive)
  (golr-sudo-prep)
  (golr-no-sudo-restart))

(defun golr-update ()
  "Starts the update from the web interface."
  (interactive)
  (let ((full-url (concat golr-solr-url "dataimport?command=full-import")))
    ;; I don't know why, but the Emacs one seems borked.
    ;;(url-retrieve-synchronously full-url)
    (browse-url full-url)
    (princ (concat "Tried to visit: " full-url "\n"))
    (princ "golr-update completed--Solr update has started!\n")))

(defun golr-cli ()
  "Command switch for command line use."
  (cond
   ((y-or-n-p "Do Solr/Jetty reset and restart: ")
    (golr-no-sudo-restart))
   (t (princ "skipping...")))
  (when (y-or-n-p "Do full data import: ")
    (golr-update)
    (princ "\n")))

;; Only run off of cli.
(let ((re "script"))
  (when (delq nil (mapcar (lambda (s) (string-match re s)) command-line-args))
    ;(princ "Detected CLI...")
    (golr-cli)))
