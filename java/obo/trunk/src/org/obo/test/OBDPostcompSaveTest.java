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
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.AnnotationUtil;
import org.obo.util.TermUtil;

public class OBDPostcompSaveTest extends AbstractAnnotationTest {

	String jdbcPath = "jdbc:postgresql://localhost:5432/obdtest";
	
	protected OBDPostcompSaveTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"sox9b_zfin.obo"};
		return Arrays.asList(files);
	}
	
	public void setUp() throws Exception {
		System.out.println("foo");
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		
		// TODO: DRY - GOAnnotationFileTest
		OBOFileAdapter adapter = new OBOFileAdapter();
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
		testPostCompClasses();
		//testForAnnotationAssignedBy("FB:FBgn0061475","GO:0005843","FlyBase");

		
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
	
	public void testPostCompClasses() {
		LinkedObject obj = (LinkedObject) session.getObject("PATO:0000462^during(ZFIN:ZDB-STAGE-010723-35)^OBO_REL:inheres_in(ZFA:0000051)");
		for (Link link : obj.getParents()) {
			System.out.println("  "+TermUtil.isIntersection(link)+" "+link.getType()+" "+link.getParent());
		}
		
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
		
		testForName("ZFIN:ZDB-GENO-070219-2",
				"Df(LG03:sox8,sox9b)b971/b971;sox9a<sup>hi1134Tg/hi1134Tg</sup>");
		testInstanceType("ZFIN:ZDB-GENO-070219-2","SO:0001027");
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		System.err.println("N annots:"+annots.size());
		for (Annotation annot : annots)
			System.out.println(annot);
		IdentifiedObject ae = session.getObject("ZFIN:ZDB-GENO-070219-2");
		Collection<Annotation> annots2 = getAnnotationsForSubject(ae);
		boolean genusFound = false;
		boolean diffFound = false;
		boolean assignedByOk = false;
		
		// check annotation. We can't do an ID check on the object as
		// it's stored as anonymous in OBD (OBD grants it a temp ID).
		// we have to check the structure of the object is the same
		Annotation match = null;
		for (Annotation annot : annots2) {
			LinkedObject obj = annot.getObject();
			for (Link link : obj.getParents()) {
				if (TermUtil.isIntersection(link)) {
					String exprObj = link.getParent().getID();
					if (link.getType().equals(OBOProperty.IS_A) &&
							exprObj.equals("PATO:0000462")) {
						genusFound = true;
					}
					else if (link.getType().getID().equals("OBO_REL:inheres_in") &&
							exprObj.equals("ZFA:0000051")) {
						diffFound = true;
					}		
				}
			}
			System.out.println("* assby="+annot.getAssignedBy());

			if (annot.getAssignedBy() != null && annot.getAssignedBy().getID().equals("ZFIN"))
				assignedByOk = true;
			if (genusFound & diffFound)
				match = annot;
		}
		assertTrue(genusFound);
		assertTrue(diffFound);
		System.out.println("assby="+match.getAssignedBy());
		System.out.println("annotation ns="+match.getNamespace());
		assertTrue(assignedByOk);
	
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
		suite.addTest(new OBDPostcompSaveTest("testHasLoaded"));
		//suite.addTest(new OBDPostcompSaveTest("testNamespaceFilteredLoad"));
	}
}



