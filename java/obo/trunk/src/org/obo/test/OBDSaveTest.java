package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.dataadapter.OBDSQLDatabaseAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;

public class OBDSaveTest extends AbstractOBOTest {

	protected String jdbcPath = "jdbc:postgresql://localhost:5432/obdtest";

	protected OBDSaveTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "nucleus.obo", "caro.obo", "camphor_catabolism.obo" };
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
		testForCategory("GO:0043227","gosubset_prok");
		testForDbxref("CARO:0000013","GO:0005623");
		testForName("GO:0005634","nucleus");
		testForSynonym("GO:0005622","protoplasm");
		testForSynonym("CHEBI:26872","fake_for_test");
	}
	
	public void testNamespaceFilteredLoad() throws DataAdapterException {
		OBDSQLDatabaseAdapterConfiguration config = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		OBDSQLDatabaseAdapter adapter = new OBDSQLDatabaseAdapter();
		
		// database -> session
		System.err.println("reading ns filtered");
		
		config.addNamespace("caro");
		config.getReadPaths().add(jdbcPath);
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
		System.err.println("read: "+session);
		testForIsA("CARO:0000003","CARO:0000006");
		testNotPresent("GO:0005622");
		
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
		suite.addTest(new OBDSaveTest("testNamespaceFilteredLoad"));

	}
}



