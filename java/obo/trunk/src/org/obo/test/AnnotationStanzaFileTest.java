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
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.PropertyValue;
import org.obo.util.AnnotationUtil;

public class AnnotationStanzaFileTest extends AbstractAnnotationTest {

	protected AnnotationStanzaFileTest(String name) {
		super(name);
	}

	public void testAnnot() throws IOException, DataAdapterException {
		Instance fred = (Instance)session.getObject("fred");
		System.out.println(fred);
		Collection<Annotation> annots = AnnotationUtil.getAnnotations(session);
		System.err.println("N annots:"+annots.size());
		for (Annotation annot : annots) {
			System.out.println(annot.getNamespace()+" annot: "+annot+":: "+annot.getSubject()+" -"+annot.getRelationship()+"-> "+annot.getObject());  
			for (PropertyValue pv : annot.getPropertyValues()) {
				System.out.println("  pv:"+pv);
			}
		}
		for(IdentifiedObject io : session.getObjects()) {
			if (!io.isBuiltIn() && !(io instanceof Annotation)) {
				System.out.println(" regular object "+io);
			}
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
		String[] files={"sox9b_zfin.obo"};
		return Arrays.asList(files);
	}
}
