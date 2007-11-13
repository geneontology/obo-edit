package org.obo.test;

import org.obo.reasoner.impl.LinkPileReasonerFactory;

import junit.framework.*;

public class AllReasonerTestsWithLPR extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new LinkPileReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		return out;
	}
}
