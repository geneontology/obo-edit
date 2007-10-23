package org.obo.test;

import java.io.*;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.GOStyleAnnotationFileAdapter;
import org.obo.dataadapter.OBDSQLDatabaseAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;


import junit.framework.*;

public class OBDSaveTest extends AbstractOBOTest {

	protected OBDSaveTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "nucleus.obo" };
		return Arrays.asList(files);
	}
	
	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		session = getSessionFromResources(getFilesToLoad());

		// SessionManager.getManager().setSession(session);
		linkDatabase = new DefaultLinkDatabase(session);
		ReasonerFactory rf = new ForwardChainingReasonerFactory();
		
		// write
		OBDSQLDatabaseAdapterConfiguration config = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		config.setSaveImplied(true);
		
		config.setWritePath("jdbc:postgresql://localhost:5432/obdtest");
		OBDSQLDatabaseAdapter adapter = new OBDSQLDatabaseAdapter();
		ReasonedLinkDatabase reasoner = rf.createReasoner();
		reasoner.setLinkDatabase(linkDatabase);
		adapter.setReasoner(reasoner);
		reasoner.recache();
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		System.err.println("saved");
		
		System.err.println("reading");
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
		System.err.println("read");
	}

	public void testHasLoaded() {
		// TODO
		testForIsA("GO:0005634","GO:0043231");
	}
	
	public boolean testForAnnotation(String su, String ob) {
		IdentifiedObject io = session.getObject(su);
		if (io != null) {
			Collection<Annotation> annots = getAnnotationsForSubject(io);
			for (Annotation annot : annots) {
				if (ob.equals(annot.getObject())) {
					return true;
				}
			}
		}
		return false;
	}
	
	public Collection<Annotation> getAnnotationsForSubject(IdentifiedObject su) {
		Collection<Annotation> annots = new LinkedList<Annotation>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof Annotation) {
				Annotation annot = (Annotation)io;
				if (su.equals(annot.getSubject())) {
					annots.add(annot);
				}
			}
		}
		return annots;
	}
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new OBDSaveTest("testHasLoaded"));
	}
}



