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
		out.addTestSuite(CardinalityTest.class);
		out.addTestSuite(CardinalityIntersectionTest.class);
		out.addTestSuite(DanglingObjectTest.class);
		out.addTestSuite(DisjointnessTest.class);
		out.addTestSuite(FilterTest.class);
		out.addTestSuite(FixedCacheMutableLinkDatabaseTest.class);
		out.addTestSuite(GOAnnotationFileTest.class);
		out.addTestSuite(GOAnnotationFilePlusOntologyTest.class);
		out.addTestSuite(IDMapperTest.class);
		out.addTestSuite(IDMapperTestWithIDLifecycle.class);
		out.addTestSuite(IDMapperTestWithReasoner.class);
		out.addTestSuite(InverseAlwaysImpliedTest.class);
		out.addTestSuite(NamespaceTest.class);
		out.addTestSuite(PostcompSyntaxFileTest.class);
		out.addTestSuite(TrimmingTest.class);
		out.addTestSuite(TrimmingLibraryTest.class);
		out.addTestSuite(WebSearchUtilTest.class);

		return out;
	}
}

