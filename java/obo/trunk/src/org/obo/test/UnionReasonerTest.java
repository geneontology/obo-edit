package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;

import org.apache.log4j.*;

public class UnionReasonerTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(UnionReasonerTest.class);

	public UnionReasonerTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"union_test.obo"};
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		// inhibition of neurotransmitter uptake IS_A negative regulation of neurotransmitter uptake
		testForIsA("a","x"); /* X = a or b ==> a is_a X */

	}

}

