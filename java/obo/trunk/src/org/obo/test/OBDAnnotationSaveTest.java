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
import org.obo.util.AnnotationUtil;


import junit.framework.*;

public class OBDAnnotationSaveTest extends AbstractAnnotationTest {

	protected OBDAnnotationSaveTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"gene_assoc.test"};
		return Arrays.asList(files);
	}
	
	public void setUp() throws Exception {
		System.out.println("foo");
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		
		// TODO: DRY - GOAnnotationFileTest
		GOStyleAnnotationFileAdapter adapter = new GOStyleAnnotationFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		for (String f : getFilesToLoad()) {
			config.getReadPaths().add(
					getResourcePath()+"/" + f);
			System.err.println(f);
		}
		config.setAllowDangling(true);
		config.setBasicSave(false);
		config.setFailFast(false);
		session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);

		String jdbcPath = "jdbc:postgresql://localhost:5432/obdtest";
		
		// SessionManager.getManager().setSession(session);
		//linkDatabase = new DefaultLinkDatabase(session);
		//ReasonerFactory rf = new ForwardChainingReasonerFactory();
		
		// write
		OBDSQLDatabaseAdapterConfiguration wconfig = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		wconfig.setSaveImplied(false);
		
		wconfig.setWritePath(jdbcPath);
		OBDSQLDatabaseAdapter wadapter = new OBDSQLDatabaseAdapter();
		//ReasonedLinkDatabase reasoner = rf.createReasoner();
		//reasoner.setLinkDatabase(linkDatabase);
		//wadapter.setReasoner(reasoner);
		//reasoner.recache();
		wadapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, wconfig, session);
		
		// database -> session
		System.err.println("reading");
		wconfig.getReadPaths().add(jdbcPath);
		session = wadapter.doOperation(OBOAdapter.READ_ONTOLOGY, wconfig, null);
		System.err.println("read: "+session);

	}

	public void testHasLoaded() {
		// TODO
		testForName("FB:FBgn0061475","18SrRNA");
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		System.err.println("N annots:"+annots.size());
		testForAnnotation("FB:FBgn0061475","GO:0005843");
	}
	
	// TODO: DRY
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
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new OBDAnnotationSaveTest("testHasLoaded"));
	}
}



