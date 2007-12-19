package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.io.AuditedPrintStream;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.reasoner.impl.TrimmedLinkDatabase;

import junit.framework.Test;
import junit.framework.TestSuite;

public class ReasonerRedundancyTest extends AbstractReasonerTest {

	public ReasonerRedundancyTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"bone.obo"};
		return Arrays.asList(files);
	}
	
	

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		AbstractReasonerTest.setReasonerFactory(new LinkPileReasonerFactory());
		//AbstractReasonerTest.setReasonerFactory(new ForwardChainingReasonerFactory());
		
		testForIsA("endochondral_bone","bone"); /* genus */
		testForIsA("tripus","bone"); /* asserted */
		testForIsA("tripus","endochondral_bone"); /* completeness */

		testForIsAInTrimmed("tripus","endochondral_bone");
		testForRedundantIsA("tripus","bone");
		
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
		suite.addTest(new ReasonerRedundancyTest("testLinks"));
	}

}

