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
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.OBOSession;
import org.obo.owl.dataadapter.OWLAdapter;

public class OWLIndividualTest extends AbstractOWLTest {

	public OWLIndividualTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "individuals.owl" };
		return Arrays.asList(files);
	}
	
	protected OBOSession getSessionFromResources(Collection<String> names)
	throws DataAdapterException {
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		for (String f : names) {
			String path;
			if (f.startsWith("/"))
				path = f;
			else
				path = getResourcePath() + "/" +f;
			config.getReadPaths().add("file:"+path);
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
	
	// TODO: DRY
	public void readOBOFile(File file) throws DataAdapterException {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(file.getAbsolutePath());
		config.setAllowDangling(true);
		config.setBasicSave(false);
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);

	}
	
	public void testHasLoaded() throws IOException, DataAdapterException {
		File f = writeTempOBOFile();
		readOBOFile(f);
		writeTempOWLFile();
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new OWLIndividualTest("testHasLoaded"));
	}
	

}



