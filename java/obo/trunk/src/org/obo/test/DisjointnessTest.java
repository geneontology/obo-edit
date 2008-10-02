package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;

import org.apache.log4j.*;

public class DisjointnessTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DisjointnessTest.class);

	public DisjointnessTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "bfo.obo"};
		return Arrays.asList(files);
	}
	

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		testForLink("snap:Continuant","disjoint_from","span:Occurrent");
		
	}

}
