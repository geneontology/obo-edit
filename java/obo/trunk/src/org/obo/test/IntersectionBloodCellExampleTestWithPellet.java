package org.obo.test;

import junit.framework.TestSuite;

import org.obo.reasoner.impl.PelletWrappedReasonerFactory;


public class IntersectionBloodCellExampleTestWithPellet extends IntersectionBloodCellExampleTest {

	protected IntersectionBloodCellExampleTestWithPellet(String name) {
		super(name);
	}

	public void setUp() throws Exception {
		setReasonerFactory(new PelletWrappedReasonerFactory());
		super.setUp();
	}
	
	public static void addTests(TestSuite suite) {
		suite.addTest(new IntersectionBloodCellExampleTestWithPellet("testSubsumed"));
	}

}

