package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
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

import org.apache.log4j.*;

public class OBDPostcompSaveTest extends AbstractAnnotationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBDPostcompSaveTest.class);

	String jdbcPath = "jdbc:postgresql://localhost:5432/obdtest";
	
	protected OBDPostcompSaveTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"sox9b_zfin.obo"};
		return Arrays.asList(files);
	}
	
	public void setUp() throws Exception {
		logger.info("foo");
		logger.info("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
		
		// TODO: DRY - GOAnnotationFileTest
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		for (String f : getFilesToLoad()) {
			config.getReadPaths().add(
					getResourcePath()+"/" + f);
			logger.info(f);
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
			logger.info("  "+TermUtil.isIntersection(link)+" "+link.getType()+" "+link.getParent());
		}
		
	}

	public void testHasLoaded() throws DataAdapterException {
		// database -> session
		logger.info("reading");
		OBDSQLDatabaseAdapterConfiguration wconfig = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		wconfig.setReadPath(jdbcPath);
		OBDSQLDatabaseAdapter wadapter = new OBDSQLDatabaseAdapter();
		session = wadapter.doOperation(OBOAdapter.READ_ONTOLOGY, wconfig, null);
		logger.info("read: "+session);
		
		testForName("ZFIN:ZDB-GENO-070219-2",
				"Df(LG03:sox8,sox9b)b971/b971;sox9a<sup>hi1134Tg/hi1134Tg</sup>");
		testInstanceType("ZFIN:ZDB-GENO-070219-2","SO:0001027");
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		logger.info("N annots:"+annots.size());
		for (Annotation annot : annots)
			logger.info(annot);
		IdentifiedObject ae = session.getObject("ZFIN:ZDB-GENO-070219-2");
		Collection<Annotation> annots2 = getAnnotationsForSubject(ae);
		boolean assignedByOk = false;
		
		// check annotation. We can't do an ID check on the object as
		// it's stored as anonymous in OBD (OBD grants it a temp ID).
		// we have to check the structure of the object is the same
		Annotation match = null;
		for (Annotation annot : annots2) {
			LinkedObject obj = annot.getObject();
			boolean genusFound = false;
			boolean diffFound = false;
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
			if (genusFound & diffFound) {
				logger.info("* assby="+annot.getAssignedBy()+" ns="+annot.getNamespace());

				if (annot.getAssignedBy() != null && annot.getAssignedBy().getID().equals("ZFIN")) 
					assignedByOk = true;
				match = annot;
			}
		}
		assertTrue(match!=null);
		logger.info("assby="+match.getAssignedBy());
		logger.info("annotation ns="+match.getNamespace());
		assertTrue(match.getNamespace().getID().equals("zfin"));
		assertTrue(match.getAssignedBy().getID().equals("ZFIN"));
		assertTrue(assignedByOk);
	
	}
	
	public void testNamespaceFilteredLoad() throws DataAdapterException {
		OBDSQLDatabaseAdapterConfiguration config = 
			new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		OBDSQLDatabaseAdapter adapter = new OBDSQLDatabaseAdapter();
		
		// database -> session
		logger.info("reading ns filtered");
		
		config.addNamespace("MGI");
		config.setReadPath(jdbcPath);
		session = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
		logger.info("read: "+session);
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		for (Annotation annot: annots)
			logger.info(annot.getSubject() + "-----" + annot.getObject());
		logger.info("N annots:"+annots.size());
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



