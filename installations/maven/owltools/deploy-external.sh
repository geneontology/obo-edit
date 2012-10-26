#!/bin/bash
###
### For deploying external non-maven libs out to our maven repo.
###

## Forester
mvn deploy:deploy-file -Dfile=./forester/forester_1005.jar -DpomFile=./forester/pom.xml  -Dsources=./forester/forester_1005-sources.jar -Djavadoc=./forester/forester_1005.javadoc.jar -DrepositoryId=BBOPDeployRepository -Durl=scp://knife.lbl.gov:/srv/www/htdocs/maven/repository
