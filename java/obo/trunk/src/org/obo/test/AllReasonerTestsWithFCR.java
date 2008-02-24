package org.obo.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.obo.reasoner.impl.ForwardChainingReasonerFactory;

public class AllReasonerTestsWithFCR extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new ForwardChainingReasonerFactory());

		TestSuite out = new TestSuite();

		//out.addTestSuite(IntersectionBloodCellExampleTest.class);
		out.addTestSuite(IntersectionCamphorCatabolismExampleTest.class);

		return out;
	}
}
