package org.blipkit.test;

import junit.framework.TestSuite;

import org.blipkit.reasoner.impl.DatalogReasonerFactory;
import org.obo.test.AbstractReasonerTest;
import org.obo.test.IntersectionBloodCellExampleTest;


public class IntersectionBloodCellExampleTestWithDatalog extends IntersectionBloodCellExampleTest {

	protected IntersectionBloodCellExampleTestWithDatalog(String name) {
		super(name);
	}

	public void setUp() throws Exception {
		AbstractReasonerTest.setReasonerFactory(new DatalogReasonerFactory());
		//setReasonerFactory(new DatalogReasonerFactory());
		super.setUp();
	}
	
	public static void addTests(TestSuite suite) {
		suite.addTest(new IntersectionBloodCellExampleTestWithDatalog("testAsserted"));
		suite.addTest(new IntersectionBloodCellExampleTestWithDatalog("testSubsumed"));
	}

}

