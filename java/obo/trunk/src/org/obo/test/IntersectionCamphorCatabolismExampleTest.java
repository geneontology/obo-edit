package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.io.AuditedPrintStream;

import junit.framework.Test;
import junit.framework.TestSuite;

public class IntersectionCamphorCatabolismExampleTest extends AbstractReasonerTest {

	public IntersectionCamphorCatabolismExampleTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"camphor_catabolism.obo"};
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		testForIsA("GO:0019383","GO:0009056"); /* genus */
		testForIsA("GO:0019383","GO:0042178"); /* completeness */
		testForLink("GO:0019383","UCDHSC:results_in_division_of","CHEBI:15396"); /* differentia */
		testForLink("GO:0019383","UCDHSC:results_in_division_of","CHEBI:35703"); /* differentia + transitivity */
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
		suite.addTest(new IntersectionCamphorCatabolismExampleTest("testLinks"));
	}

}

