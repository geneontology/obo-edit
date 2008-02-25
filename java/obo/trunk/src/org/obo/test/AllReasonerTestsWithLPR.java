package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.LinkPileReasonerFactory;

public class AllReasonerTestsWithLPR extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new LinkPileReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);
		out.addTestSuite(IntersectionUsingSubRelationsTest.class);

		return out;
	}
}
