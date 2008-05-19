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
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.datamodel.impl.NCBOOboInOWLMetadataMapping;

import org.apache.log4j.*;

public class NCBOStyleOWLTest extends AbstractOWLTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NCBOStyleOWLTest.class);

	public NCBOStyleOWLTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "cell.owl" };
		return Arrays.asList(files);
	}
	
	protected OBOSession getSessionFromResources(Collection<String> names)
	throws DataAdapterException {
		OWLAdapter adapter = new OWLAdapter();
		OWLAdapter.OWLAdapterConfiguration config = new OWLAdapter.OWLAdapterConfiguration();
		config.addMetadataMapping(new NCBOOboInOWLMetadataMapping());
		for (String f : names) {
			String path;
			if (f.startsWith("/"))
				path = f;
			else
				path = getResourcePath() + "/" +f;
			config.getReadPaths().add(path);
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
		File f = writeTempOBOFile();
		readOBOFile(f);
		writeTempOWLFile();
	}
	
	public void testContents() {
		testForName("CL:0000134", "mesenchymal cell");
		for (IdentifiedObject io : session.getObjects()) {
			if (io.getID().equals("CL:0000134"))
				assertTrue(io.getName().equals("mesenchymal cell"));
		}
		IdentifiedObject io = session.getObject("CL:0000335");
		for (Link link : ((LinkedObject)io).getParents()) {
			LinkedObject p = link.getParent();
			OBOProperty t = link.getType();
			assertTrue(p.getName().equals("mesenchymal cell"));
			assertTrue(t.isTransitive());
			assertTrue(t.getName().equals("develops_from"));
		}
		io = session.getObject("CL:0000000");
		boolean ok = false;
		for (Link link : ((LinkedObject)io).getChildren()) {
			LinkedObject c = link.getChild();
			OBOProperty t = link.getType();
			logger.info(link);
			if (c.getName().equals("cell in vivo"))
				ok = true;
		}
		assertTrue(ok);

	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new NCBOStyleOWLTest("testContents"));
		suite.addTest(new NCBOStyleOWLTest("testHasLoaded"));
	}
	

}



