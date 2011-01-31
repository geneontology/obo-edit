RUNNING ON THE COMMAND LINE:

This is the simplest way; no external dependencies are required. Just launch:

	./launch_scripts/run-amiso-server -i ~/cvs/song/ontology/so.obo -p 8187

Or a database:

	./launch_scripts/run-amiso-server -d jdbc:postgresql://localhost:5432/obdtest
	
You can pass as many -i and -d arguments as you like - the application will (mostly)
seamlessly wrap them all

Now point yr browser at

	http://localhost:8187
	
RUNNING FROM ECLIPSE:

First set up project dependencies:

 * BBOP
 * OBO
 * OBDAPI
 
(or include the jars)

Then right-click AmigoRestApplication and choose Run

In the Arguments tab, enter

	-i ~/cvs/song/ontology/so.obo -p 8187

(or as many files and databases as you like)

EXTENDING THE CODE:

You are best to do this from eclipse. For templates, javascript, css etc see:

	org.geneontology.web.pages
	
Make sure you have eclipse set up to build in the classes/ directory

If you do not use eclipse you need to make sure css/js/ftl templates etc get placed in the
classes/ dir (I have not yet figured out how to make ant do this)

Use ant to build the jar - this will copy the contents of the classes/ dir to the jar - this should
includes pages/{js,css,templates,images} etc too

You can go a long way just mucking with templates. To actually add new resources, see the AmigoRestApplication
java file. 
