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
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping;

public class SubclassAxiomAnnotationTest extends AbstractOWLTest {

	protected SubclassAxiomAnnotationTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "annotated_is_a_example.obo" };
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
		
		
	}

	


	public void testHasLoaded() throws IOException, DataAdapterException {
		File outFile = writeTempOWLFile(new NCBOOboInOWLMetadataMapping());
		//outFile = writeTempOWLFile();
		//outFile = writeTempOWLFile();
		readOWLFile(outFile);
		writeTempOBOFile();
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new SubclassAxiomAnnotationTest("testHasLoaded"));
		suite.addTest(new SubclassAxiomAnnotationTest("testLinks"));
	}
	

}



