package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.rbr.RuleBasedReasonerFactory;

import org.apache.log4j.*;

public class AllReasonerTests extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AllReasonerTests.class);

	public static Test suite() {

		TestSuite out = new TestSuite();

		out.addTestSuite(TransitivityTest.class);
		out.addTestSuite(TrimmingTest.class);
		out.addTestSuite(SubRelationReasonerTest.class);
		out.addTestSuite(DisjointnessTest.class);
		out.addTestSuite(DisjointnessTest2.class);

		out.addTestSuite(HoldsOverChainReasonerTest.class);
		out.addTestSuite(RedundancyTest.class);

		out.addTestSuite(ReasonerRedundancyTest.class);

		out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		
		return out;
	}
}
