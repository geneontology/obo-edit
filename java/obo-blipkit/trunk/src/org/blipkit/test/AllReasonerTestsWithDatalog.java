package org.blipkit.test;

import org.blipkit.reasoner.impl.DatalogReasonerFactory;
import org.obo.test.AbstractReasonerTest;
import org.obo.test.IntersectionBloodCellExampleTest;
import org.obo.test.IntersectionCamphorCatabolismExampleTest;

import junit.framework.*;

public class AllReasonerTestsWithDatalog extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new DatalogReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		return out;
	}
}
