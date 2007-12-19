package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.io.AuditedPrintStream;
import org.obo.reasoner.impl.PelletWrappedReasonerFactory;

import junit.framework.Test;
import junit.framework.TestSuite;


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

