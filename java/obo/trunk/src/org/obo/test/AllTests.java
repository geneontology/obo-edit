package org.obo.test;



import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


public class AllTests extends TestCase {

	protected AllTests(String name) {
		super(name);
	}
	
	public static Test suite() {
		TestSuite out = new TestSuite();

		out.addTestSuite(IDUpdateTest.class);
		out.addTestSuite(AllReasonerTestsWithLPR.class);
		out.addTestSuite(AnnotationStanzaFileTest.class);
		out.addTestSuite(FixedCacheMutableLinkDatabaseTest.class);
		out.addTestSuite(DisjointnessTest.class);
		out.addTestSuite(GOAnnotationFileTest.class);
		out.addTestSuite(GOAnnotationFilePlusOntologyTest.class);
		out.addTestSuite(PostcompSyntaxFileTest.class);
		out.addTestSuite(TrimmingTest.class);

		return out;
	}
}

