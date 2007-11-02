package org.obo.test;

import org.obo.reasoner.impl.PelletWrappedReasonerFactory;

import junit.framework.*;
import org.obo.test.*;

public class AllReasonerTestsWithPellet extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new PelletWrappedReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(IntersectionBloodCellExampleTest.class);
		
		// we don't expect this one to succeed yet:
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		return out;
	}
}
