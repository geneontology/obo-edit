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
import org.obo.datamodel.OBOSession;
import org.obo.owl.dataadapter.OWLAdapter;

public class BioTopRoundtripOWLTest extends AbstractOWLTest {

	protected BioTopRoundtripOWLTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "http://purl.org/biotop/dev" };
		return Arrays.asList(files);
	}
	
	protected OBOSession getSessionFromResources(Collection<String> names)
	throws DataAdapterException {
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		for (String f : names) {
			config.getReadPaths().add(f);
		}
		config.setAllowLossy(true);
		session = adapter.doOperation(OWLAdapter.READ_ONTOLOGY, config,
				null);
		return session;
	}

	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}
	
	public void testHasLoaded() throws IOException, DataAdapterException {
		testForIsA("biotop_dev:AminoGroup","biotop_dev:FunctionalGroup");
		File f = writeTempOBOFile();
		readOBOFile(f);
		writeTempOWLFile();
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new BioTopRoundtripOWLTest("testHasLoaded"));
	}
	

}



