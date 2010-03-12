package org.obo.owl.test;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.OBOProperty;
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping;

import org.apache.log4j.*;

public class BasicOWLTest extends AbstractOWLTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BasicOWLTest.class);

	public BasicOWLTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "nucleus.obo", "bfo.obo", "camphor_catabolism.obo", "part_of_test.obo", "regulation_of_transcription_xp.obo" };
		return Arrays.asList(files);
	}
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}
	
	public void testLinks() throws Exception {
		testForIsA("CHEBI:33304","CHEBI:33675"); /* asserted */
		testForLink("testA","part_of","testB"); /* asserted */
		testForIsTransitive("part_of");
		testForLink("prok_cell",OBOProperty.UNION_OF.getID(),"arc_cell"); /* asserted */
		
	}

	


	public void testHasLoaded() throws IOException, DataAdapterException {
		File outFile = writeTempOWLFile(new NCBOOboInOWLMetadataMapping());
		//outFile = writeTempOWLFile();
		//outFile = writeTempOWLFile();
		readOWLFile(outFile);
		writeTempOBOFile();
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new BasicOWLTest("testHasLoaded"));
		suite.addTest(new BasicOWLTest("testLinks"));
	}
	

}



