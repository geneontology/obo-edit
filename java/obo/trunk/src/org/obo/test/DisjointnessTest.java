package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.impl.PelletWrappedReasonerFactory;

public class DisjointnessTest extends AbstractReasonerTest {

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

	public static Test suite() {
		System.out.println("foo");
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		addTests(suite);
		return suite;
	}

	public static void addTests(TestSuite suite) {
		suite.addTest(new DisjointnessTest("testLinks"));
	}
}
