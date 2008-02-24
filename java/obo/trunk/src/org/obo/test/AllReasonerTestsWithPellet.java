package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.PelletWrappedReasonerFactory;

public class AllReasonerTestsWithPellet extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new PelletWrappedReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(TransitivityTest.class);

		out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		
		return out;
	}
}
