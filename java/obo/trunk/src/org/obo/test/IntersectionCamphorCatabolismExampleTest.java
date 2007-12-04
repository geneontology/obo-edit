package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.io.AuditedPrintStream;
import org.obo.reasoner.impl.TrimmedLinkDatabase;

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
		testForIsA("CHEBI:24974","CHEBI:23367"); /* is_a transitivity */
		testForIsA("CHEBI:33304","CHEBI:33675"); /* asserted */
		testForIsA("GO:0019383","GO:0009056"); /* genus */
		testForIsA("GO:0019383","GO:0042178"); /* completeness */
		testForLink("testA","part_of","testB"); /* asserted */
		testForLink("testA","part_of","testC"); /* transitivity */
		testForLink("GO:0019383","UCDHSC:results_in_division_of","CHEBI:35703"); /* differentia + transitivity */

		testForIsAInTrimmed("GO:0019383","GO:0042178"); 
		testForLinkInTrimmed("GO:0019383","UCDHSC:results_in_division_of","CHEBI:35703"); /* differentia + transitivity */
		
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

