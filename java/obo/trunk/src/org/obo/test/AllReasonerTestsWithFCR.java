package org.obo.test;

import org.obo.reasoner.impl.ForwardChainingReasonerFactory;

import junit.framework.*;

public class AllReasonerTestsWithFCR extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new ForwardChainingReasonerFactory());

		TestSuite out = new TestSuite();

		//out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		return out;
	}
}
