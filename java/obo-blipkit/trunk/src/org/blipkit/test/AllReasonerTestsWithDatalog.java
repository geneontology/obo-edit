package org.blipkit.test;

import org.blipkit.reasoner.impl.DatalogReasonerFactory;
import org.obo.test.AbstractReasonerTest;
import org.obo.test.DisjointnessTest;
import org.obo.test.DisjointnessTest2;
import org.obo.test.HoldsOverChainReasonerTest;
import org.obo.test.IntersectionBloodCellExampleTest;
import org.obo.test.IntersectionCamphorCatabolismExampleTest;
import org.obo.test.RedundancyTest;
import org.obo.test.SubRelationReasonerTest;
import org.obo.test.TransitivityTest;
import org.obo.test.TrimmingTest;

import junit.framework.*;

public class AllReasonerTestsWithDatalog extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new DatalogReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);
		out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(TransitivityTest.class);
		out.addTestSuite(TrimmingTest.class);
		out.addTestSuite(SubRelationReasonerTest.class);
		out.addTestSuite(DisjointnessTest.class);
		out.addTestSuite(DisjointnessTest2.class);

		out.addTestSuite(HoldsOverChainReasonerTest.class);
		out.addTestSuite(RedundancyTest.class);

		return out;
	}
}
