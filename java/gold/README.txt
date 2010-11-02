###For installation please refer to INSTALL.txt file

###Hibernate documentation

Following steps are for the hibernate reverse engineering (generating java class from the database schema):

STEP 1: Install hibernate tools in eclipse.
			Click on the Help menu option in the eclipse environment and
			select "Install New Software option".
			Add the http://download.jboss.org/jbosstools/updates/development location to download hibernate.
			From this repository select the Under the All JBoss Tools select the "Hibernate tools"
			and hit next button to follow the wizard to install the tools.
STEP 2: update the database information in the file at conf/hibernate.cfg.xml
		according to your environment.
STEP 3: To initiate reverse engineering click on the 
		Run->Hibernat Code Generation->Hibernat Code Generation Configurations menu item. 


For more detail about hibernate reverse engineering please refer to the 
following links:

	- http://www.wikihow.com/Generate-Hibernate-Pojo-Classes-from-DB-Tables
	- http://docs.jboss.org/tools/3.1.0.GA/en/hibernatetools/html/reverseengineering.html
	- http://docs.jboss.org/tools/3.1.0.GA/en/hibernatetools/html/plugins.html