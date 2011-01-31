Assuming that everything else is ok (policy and shared jars in proper
places), to install this under tomcat:

*) Link /usr/share/tomcat5.5/webapps/RefGenomeService to Eclipse
 working compile directory
 (e.g. /srv/www/htdocs/refgen/gwt/www/org.bbop.RefGenome ).

*) Create WEB-INFS directory in
 /usr/share/tomcat5.5/webapps/RefGenomeService and copy in web.xml from
 svn:/geneontology/go-dev/gwt/WEB-INFs/RefGenomeService .

*) Create classes in WEB-INFs and copy in class tree from
 /srv/www/htdocs/refgen/gwt/bin .
