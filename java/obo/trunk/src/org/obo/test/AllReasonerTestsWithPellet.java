package org.obo.test;

import org.obo.reasoner.impl.PelletWrappedReasonerFactory;

import junit.framework.*;

public class AllReasonerTestsWithPellet extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new PelletWrappedReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(TransitivityTest.class);

		//out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		
		return out;
	}
}
