#!/bin/bash
###
### For bringing in non-maven libs locally.
###

## Forester
mvn install:install-file -Dfile=./forester/forester_1005.jar -DpomFile=./forester/pom.xml  -Dsources=./forester/forester_1005-sources.jar -Djavadoc=./forester/forester_1005.javadoc.jar
