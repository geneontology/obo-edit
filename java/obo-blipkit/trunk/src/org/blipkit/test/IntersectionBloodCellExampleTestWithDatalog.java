package org.blipkit.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.blipkit.reasoner.impl.DatalogReasonerFactory;
import org.obo.test.AbstractReasonerTest;
import org.obo.test.IntersectionBloodCellExampleTest;
import org.obo.test.IntersectionCamphorCatabolismExampleTest;


public class IntersectionBloodCellExampleTestWithDatalog extends TestCase {

	public static Test suite() {

		AbstractReasonerTest.setReasonerFactory(new DatalogReasonerFactory());

		TestSuite out = new TestSuite();

		out.addTestSuite(IntersectionBloodCellExampleTest.class);

		return out;
	}

}

