package org.obo.test;

import junit.framework.TestSuite;

import org.obo.reasoner.impl.PelletWrappedReasonerFactory;


import org.apache.log4j.*;

public class IntersectionBloodCellExampleTestWithPellet extends IntersectionBloodCellExampleTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionBloodCellExampleTestWithPellet.class);

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

