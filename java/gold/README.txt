###For installation please refer to INSTALL.txt file

####Command Line Interace (CLI)
For the project home directory run the following command to find
the help about the usage of the command line interface by running the following cammands:

1) bin/gold-runner
2) bin/jetty-runner
3) java -jar gold.jar

####Web Application Interface
The application contains the pre-configured jetty server. The jetty server can
be run by the 'bin/jetty-runner start' command. The main web application interface is located at
http://localhost:8888

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
	
###Solr

Solr is pre-configured in the project. To access the solr admin interface visit the url http://localhost:8080/solr/admin.
Data can be imported in two ways in the solr as:

1) 	Through owl file: To load from OWL file change the configuration under this tag:  <requestHandler name="/dataimport"
	in the solr/conf/solrconfig.xml file. See comments under the tag how to configure it.
2)	Through rdbms: See comments under the tag <requestHandler name="/dataimport"
	in the solr/conf/solrconfig.xml file for detail how to configure this option.
	
	