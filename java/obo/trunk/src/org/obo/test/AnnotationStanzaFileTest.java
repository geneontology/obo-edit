package org.obo.test;

import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.io.AuditedPrintStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.util.AnnotationUtil;

public class AnnotationStanzaFileTest extends AbstractAnnotationTest {

	protected AnnotationStanzaFileTest(String name) {
		super(name);
	}

	public void testAnnot() throws IOException, DataAdapterException {
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		System.err.println("N annots:"+annots.size());
		for (Annotation annot : annots) {
			System.err.println("annot: "+annot+":: "+annot.getSubject()+" -"+annot.getRelationship()+"-> "+annot.getObject());  
		}
		
		writeTempOBOFile();
		testForAnnotation("fred","bread");
		assertTrue(true);
	}
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new AnnotationStanzaFileTest("testAnnot"));
	}


	@Override
	public Collection<String> getFilesToLoad() {
		String[] files={"simple-annot-stanza-example.obo"};
		return Arrays.asList(files);
	}
}
