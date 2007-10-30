package org.obo.test;

import java.io.*;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.GOStyleAnnotationFileAdapter;
import org.obo.dataadapter.OBDSQLDatabaseAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
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
		String[] files = { "nucleus.obo", "caro.obo" };
		return Arrays.asList(files);
	}
	
	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		
		// file -> session
		session = getSessionFromResources(getFilesToLoad());

		// SessionManager.getManager().setSession(session);
		linkDatabase = new DefaultLinkDatabase(session);
		ReasonerFactory rf = new ForwardChainingReasonerFactory();
		
		// session -> database
		OBDSQLDatabaseAdapterConfiguration config = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		config.setSaveImplied(true);
		
		String jdbcPath = "jdbc:postgresql://localhost:5432/obdtest";
		config.setWritePath(jdbcPath);
		OBDSQLDatabaseAdapter adapter = new OBDSQLDatabaseAdapter();
		ReasonedLinkDatabase reasoner = rf.createReasoner();
		reasoner.setLinkDatabase(linkDatabase);
		adapter.setReasoner(reasoner);
		reasoner.recache();
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		System.err.println("saved");
		
		// database -> session
		System.err.println("reading");
		config.getReadPaths().add(jdbcPath);
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
		System.err.println("read: "+session);
		
	}

	public void testHasLoaded() {
		testForIsA("GO:0005634","GO:0043231");
		testForLink("GO:0044464","part_of","GO:0005623");
		testForNamespace("GO:0044464", "cellular_component");
		testForDefinition("GO:0005623", "The basic structural and functional unit of all organisms. Includes the plasma membrane and any external encapsulating structures such as the cell wall and cell envelope.");
		
	}
	
	public void testFileSave() throws DataAdapterException {
		// session -> file
		OBOFileAdapter fileAdapter = new OBOFileAdapter();
		OBOAdapterConfiguration fileConfig = new OBOFileAdapter.OBOAdapterConfiguration();
		OBOSerializationEngine.FilteredPath path = new OBOSerializationEngine.FilteredPath();
		path.setUseSessionReasoner(false);
		path.setPath("foo.obo");
		fileConfig.getSaveRecords().add(path);
		fileConfig.setBasicSave(false);
		fileConfig.setSerializer("OBO_1_2");
		fileAdapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, fileConfig, session);
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
		suite.addTest(new OBDSaveTest("testFileSave"));
	}
}



