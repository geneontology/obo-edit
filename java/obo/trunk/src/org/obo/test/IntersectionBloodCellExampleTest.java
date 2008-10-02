package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;


import org.apache.log4j.*;

public class IntersectionBloodCellExampleTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionBloodCellExampleTest.class);

	public IntersectionBloodCellExampleTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"zf_nucleate_erythrocyte.obo",
				"nucleus.obo"};
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testSubsumed() throws Exception {
		testForIsA("ZF:erythrocyte","CL:nucleate_erythrocyte");
	}

	public void testAsserted() throws Exception {
		testForIsA("GO:0043226", "GO:0005575");
	}


	
}

