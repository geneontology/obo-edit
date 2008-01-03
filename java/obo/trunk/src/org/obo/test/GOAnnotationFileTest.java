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

public class GOAnnotationFileTest extends AbstractAnnotationTest {

	public GOAnnotationFileTest(String name) {
		super(name);
	}
	
	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"gene_assoc.test"};
		return Arrays.asList(files);
	}

	String outPath;

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
		

		// SessionManager.getManager().setSession(session);
		linkDatabase = new DefaultLinkDatabase(session);
	}
	
	public void loadAnnotations(String f) throws DataAdapterException {
		GOStyleAnnotationFileAdapter adapter = new GOStyleAnnotationFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(f);
		session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);
	}
	
	public void testRoundTrip() throws DataAdapterException, IOException {
		// write
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		File outFile = File.createTempFile("foo", "bar");
		//outFile.deleteOnExit();
		outPath = outFile.getAbsolutePath();
		config.setWritePath(outPath);
		GOStyleAnnotationFileAdapter adapter = new GOStyleAnnotationFileAdapter();
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, session);
		loadAnnotations(outPath);
		testAnnot();
	}

	public void testAnnot() {
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		for (Annotation annot : annots)
			System.out.println(annot);
		System.err.println("N annots:"+annots.size());
		
		testForName("FB:FBgn0061475","18SrRNA");
		testForAnnotation("FB:FBgn0024177","GO:0005921");
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
		suite.addTest(new GOAnnotationFileTest("testRoundTrip"));
	}


}
