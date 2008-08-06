package org.obo.test;



import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


import org.apache.log4j.*;

public class AllTests extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllTests.class);

	protected AllTests(String name) {
		super(name);
	}
	
	public static Test suite() {
		TestSuite out = new TestSuite();

		out.addTestSuite(AllReasonerTestsWithLPR.class);
		out.addTestSuite(AnnotationStanzaFileTest.class);
		out.addTestSuite(CardinalityTest.class);
		out.addTestSuite(CardinalityIntersectionTest.class);
		out.addTestSuite(DanglingObjectTest.class);
		out.addTestSuite(DisjointnessTest.class);
		out.addTestSuite(DisjointnessTest2.class);
		out.addTestSuite(FilterTest.class);
		out.addTestSuite(FixedCacheMutableLinkDatabaseTest.class);
		out.addTestSuite(GOAnnotationFileTest.class);
		out.addTestSuite(GOAnnotationFilePlusOntologyTest.class);
		out.addTestSuite(HoldsOverChainReasonerTest.class);
		out.addTestSuite(IDMapperTest.class);
		out.addTestSuite(IDMapperTestWithIDLifecycle.class);
		out.addTestSuite(IDMapperTestWithReasoner.class);
		out.addTestSuite(IDUpdateTest.class);
		out.addTestSuite(InverseAlwaysImpliedTest.class);
		out.addTestSuite(NamespaceTest.class);
		out.addTestSuite(PostcompSyntaxFileTest.class);
		out.addTestSuite(PropertyIntersectionTest.class);
		out.addTestSuite(ReasonerRedundancyTest.class);
		out.addTestSuite(RedundancyTest.class);
		out.addTestSuite(RemoveRedundantLinksTest.class);
		out.addTestSuite(TrimmingTest.class);
		out.addTestSuite(TrimmingLibraryTest.class);
		out.addTestSuite(WebSearchUtilTest.class);

		return out;
	}
}

