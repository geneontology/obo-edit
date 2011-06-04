#
# Regular cron jobs for the geneontology-amigo-install package
#
0 4	* * *	root	[ -x /usr/bin/geneontology-amigo-install_maintenance ] && /usr/bin/geneontology-amigo-install_maintenance
