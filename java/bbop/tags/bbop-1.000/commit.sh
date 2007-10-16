#!/bin/bash
#Initialize basic program paths
ANT_PATH=ant
SVN_PATH=/usr/local/bin/svn

#File location variables are usually set by ant before this script is called
#but if they haven't been, set them to the default values
if [ -z "$JARFILE" ]; then
	JARFILE=bbop.jar
fi
if [ -z "$JAR_MANIFEST" ]; then
	JAR_MANIFEST=jar_manifest
fi
if [ -z "$SOURCEDIR" ]; then
	SOURCEDIR=src
fi
if [ -z "$LIBRARYDIR" ]; then
	LIBRARYDIR=lib
fi
if [ -z "$RELEASENOTES" ]; then
	RELEASENOTES=CHANGES
fi
if [ -z "$USERNAME" ]; then
	USERNAME=$USER
fi

#Clean the project before we commit so we don't accidentally commit files that
#can be automatically generated
${ANT_PATH} clean

#Read the current library version from the jar manifest file
while read myline
do
  VERSION=${myline#Implementation-Version: }
  if [ -z "$VERSION" ]; then
  	break;
  fi
done < $JAR_MANIFEST

#Add all new paths in the source directory
#find ${LIBRARYDIR} -type d -exec ${SVN_PATH} add {} \;

#Add all new paths in the libraries directory
#find ${SOURCEDIR} -type d -exec ${SVN_PATH} add {} \;

#Add all new jar files in the library directories
#find ${LIBRARYDIR} -name "*.jar" -exec ${SVN_PATH} add {} \;

#Add all new source files in the source directories
#find ${SOURCEDIR} -name "*.java" -exec ${SVN_PATH} add {} \;

#Commit the files
${SVN_PATH} commit --username ${USERNAME} --password $1 -F ${RELEASENOTES}