package org.obo.test;

import java.io.*;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.GOStyleAnnotationFileAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.AnnotationUtil;


import junit.framework.*;

public class GOAnnotationFileTest extends AbstractAnnotationTest {

	protected GOAnnotationFileTest(String name) {
		super(name);
	}

	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ForwardChainingReasoner.checkRecache = false;
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
		
		// write
		config = new OBOFileAdapter.OBOAdapterConfiguration();
		File outFile = File.createTempFile("foo", "bar");
		//outFile.deleteOnExit();
		config.setWritePath(outFile.getAbsolutePath());
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);

		// SessionManager.getManager().setSession(session);
		linkDatabase = new DefaultLinkDatabase(session);
	}

	public void testAnnot() {
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		System.err.println("N annots:"+annots.size());
		
		testForAnnotation("FB:FBgn0024177","GO:0005921");
		testForName("FB:FBgn0061475","18SrRNA");
		testForAnnotationAssignedBy("FB:FBgn0061475","GO:0005843","FlyBase");
		testForAnnotationPublication("FB:FBgn0061475","GO:0005843","FB:FBrf0121292");
		testForAnnotationWithEvidenceCode("FB:FBgn0061475","GO:0005843","ISS");
		testForNamespace("FB:FBgn0061475","FB");
		testForLink("FB:FBgn0061475","has_taxon","taxon:7227");
	}
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new GOAnnotationFileTest("testAnnot"));
	}


	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"gene_assoc.test"};
		return Arrays.asList(files);
	}
}
