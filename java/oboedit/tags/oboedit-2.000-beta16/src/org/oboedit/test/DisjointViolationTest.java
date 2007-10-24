package org.oboedit.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.test.AbstractReasonerTest;
import org.oboedit.verify.OntologyCheck;
import org.oboedit.verify.impl.DisjointednessCheck;

public class DisjointViolationTest extends AbstractReasonerTest {

	protected DisjointViolationTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"bfo.obo","disjoint_test_for_bfo.obo"};
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		OntologyCheck check = new DisjointednessCheck();
		System.out.println(check);
		assertTrue(true); // TODO
		//testForUnsatisfiable("unsatisfiable");
	}

	public static Test suite() {
		System.out.println("foo");
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new DisjointViolationTest("testLinks"));
	}

}

