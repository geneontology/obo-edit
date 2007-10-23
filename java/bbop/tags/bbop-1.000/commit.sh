#!/bin/bash
#Initialize basic program paths if they aren't already set
if [ -z "$SVN_PATH" ]; then
    SVN_PATH=/usr/local/bin/svn
fi
if [ -z "$ANT_PATH" ]; then
    ANT_PATH=ant
fi

#Initialize file location variables if they aren't already set
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
if [ -z "$SVNREPOSITORY" ]; then
	SVNREPOSITORY=https://geneontology.svn.sourceforge.net/svnroot/geneontology/java/bbop
fi
if [ -z "$TAGPREFIX" ]; then
    TAGPREFIX="bbop-"
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

#Get the list of files that SVN has in its database...
EXPECTED_FILES=(`find . -regex ".*/\.svn/text-base/.*\.svn-base" | sed 's/\(.*\)\.svn\/text-base\/\(.*\)\.svn-base/\1\2/'`)

  #Check each of those files to see if it's actually on disk
  for file in ${EXPECTED_FILES[@]}
  do
     if [ ! -e $file ]; then
       #If it isn't, do a SVN delete 
   	   ${SVN_PATH} delete $file
     fi
  done

#Add all new paths in the libraries directory
find ${LIBRARYDIR} -type d ! -regex ".*\.svn.*" -exec ${SVN_PATH} add -q {} \;

#Add all new paths in the source directory
find ${SOURCEDIR} -type d ! -regex ".*\.svn.*" -exec ${SVN_PATH} add -q {} \;

#Add all new jar files in the library directories
find ${LIBRARYDIR} -name "*.jar" -exec ${SVN_PATH} add -q {} \;

#Add all new source files in the source directories
find ${SOURCEDIR} -name "*.java" -exec ${SVN_PATH} add -q {} \;

#Add all new resources in the source directories
find ${SOURCEDIR} -regex ".*/resources/.*" -exec ${SVN_PATH} add -q {} \;

#Commit the files
${SVN_PATH} commit --username ${USERNAME} --password $1 -F ${RELEASENOTES}

#Delete any old tags
${SVN_PATH} --username ${USERNAME} --password $1 delete ${SVNREPOSITORY}/tags/${TAGPREFIX}${VERSION} -m "Removing old tag ${TAGPREFIX}${VERSION}"

#Tag the release
${SVN_PATH} --username ${USERNAME} --password $1 copy ${SVNREPOSITORY}/trunk ${SVNREPOSITORY}/tags/${TAGPREFIX}${VERSION} -m "Tagging version ${TAGPREFIX}${VERSION}"