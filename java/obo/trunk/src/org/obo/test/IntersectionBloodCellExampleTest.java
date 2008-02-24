package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;


public class IntersectionBloodCellExampleTest extends AbstractReasonerTest {

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

	public void testAsserted() throws Exception {
		testForIsA("GO:0043226", "GO:0005575");
	}

	public void testSubsumed() throws Exception {
		testForIsA("ZF:erythrocyte","CL:nucleate_erythrocyte");
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
		suite.addTest(new IntersectionBloodCellExampleTest("testAsserted"));
		suite.addTest(new IntersectionBloodCellExampleTest("testSubsumed"));
	}

}

