package org.obo.test;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.GOStyleAnnotationFileAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.AnnotationUtil;

public class GOAnnotationFilePlusOntologyTest extends GOAnnotationFileTest {

	protected GOAnnotationFilePlusOntologyTest(String name) {
		super(name);
	}
	
	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"gene_assoc.test"};
		return Arrays.asList(files);
	}
	public Collection<String> getOntologyFilesToLoad() {
//		String[] files={"http://purl.org/obo/obo/GO"};
		String[] files={"/users/cjm/cvs/go/ontology/gene_ontology_edit.obo"};
		return Arrays.asList(files);
	}

	String outPath;

	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		session = getSessionFromResources(getOntologyFilesToLoad());
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
		session.importSession((OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null), true);
		

		// SessionManager.getManager().setSession(session);
		linkDatabase = new DefaultLinkDatabase(session);
	}
	

	public void testAnnotWithGO() {
		Collection<Annotation> annots = getAnnotationsForSubject("FB:FBgn0024177");
		for (Annotation annot : annots) {
			System.out.println(annot.getObject().getName());
		}
		
	}
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new GOAnnotationFilePlusOntologyTest("testAnnot"));
		suite.addTest(new GOAnnotationFilePlusOntologyTest("testAnnotWithGO"));
		//suite.addTest(new GOAnnotationFilePlusOntologyTest("testRoundTrip"));
	}


}
