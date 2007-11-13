package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

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
import org.obo.datamodel.OBOSession;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.AnnotationUtil;

public class OBDAnnotationSaveTest extends AbstractAnnotationTest {

	String jdbcPath = "jdbc:postgresql://localhost:5432/obdtest";
	
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
		testForAnnotationAssignedBy("FB:FBgn0061475","GO:0005843","FlyBase");

		
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
		

	}

	public void testHasLoaded() throws DataAdapterException {
		// database -> session
		System.err.println("reading");
		OBDSQLDatabaseAdapterConfiguration wconfig = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		wconfig.getReadPaths().add(jdbcPath);
		OBDSQLDatabaseAdapter wadapter = new OBDSQLDatabaseAdapter();
		session = wadapter.doOperation(OBOAdapter.READ_ONTOLOGY, wconfig, null);
		System.err.println("read: "+session);
		
		testForName("FB:FBgn0061475","18SrRNA");
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		System.err.println("N annots:"+annots.size());
		testForAnnotation("FB:FBgn0061475","GO:0005843");
		testForAnnotation("FB:FBgn0024177","GO:0005921");
		testForNamespace("FB:FBgn0061475","FB");
		testForLink("FB:FBgn0061475","has_taxon","taxon:7227");
		testForAnnotationPublication("FB:FBgn0061475","GO:0005843","FB:FBrf0121292");
		testForAnnotationWithEvidenceCode("FB:FBgn0061475","GO:0005843","ISS");
		testForAnnotationAssignedBy("FB:FBgn0061475","GO:0005843","FlyBase");
	
	}
	
	public void testNamespaceFilteredLoad() throws DataAdapterException {
		OBDSQLDatabaseAdapterConfiguration config = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		OBDSQLDatabaseAdapter adapter = new OBDSQLDatabaseAdapter();
		
		// database -> session
		System.err.println("reading ns filtered");
		
		config.addNamespace("MGI");
		config.getReadPaths().add(jdbcPath);
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
		System.err.println("read: "+session);
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		for (Annotation annot: annots)
			System.out.println(annot.getSubject() + "-----" + annot.getObject());
		System.err.println("N annots:"+annots.size());
		testFileSave("mgi-filtered");
		
		testForName("MGI:MGI:95723","Gjb5");
		testForAnnotation("MGI:MGI:95723","GO:0016020");
		testForAnnotationAssignedBy("MGI:MGI:95723","GO:0016020","UniProt");
		testNotPresent("FB:FBgn0061475");
		
	}
	

	
	// TODO: DRY
	public void testFileSave(String base) throws DataAdapterException {
		// session -> file
		OBOFileAdapter fileAdapter = new OBOFileAdapter();
		OBOAdapterConfiguration fileConfig = new OBOFileAdapter.OBOAdapterConfiguration();
		OBOSerializationEngine.FilteredPath path = new OBOSerializationEngine.FilteredPath();
		path.setUseSessionReasoner(false);
		path.setPath(base+".obo");
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
		suite.addTest(new OBDAnnotationSaveTest("testNamespaceFilteredLoad"));
	}
}



