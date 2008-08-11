The following steps should allow us to run the RefGenome application without the rsync
scheme.
-----------------------------------------------------------------------------------


* Assuming you have a fresh rollout from subversion. If not please update.

* Please download the latest version of gwt-1.5

* Define GWT_HOME environmental variable.
	For bash shell ....
	export GWT_HOME=/home/user/gwt-1.5
	(Avoid the forward slash at the end)

* Run 'ant -f project.ant.xml' from the top of the folder.
  This will compile all sources in 'bin' folder. The server side java classes needs to be
  compiled for doing the server side request in the hosted mode.

* Run 'RefGenome-shell'. 
  It should start up the application in the hosted mode.
