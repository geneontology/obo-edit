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

public class InverseAlwaysImpliedTest extends AbstractReasonerTest {

	protected InverseAlwaysImpliedTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "inverse_always_implied.obo" };
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		for(IdentifiedObject io : reasonedDB.getObjects()) {
			if (io instanceof LinkedObject) {
				System.err.println("parents of "+io);
				for(Link link : reasonedDB.getParents((LinkedObject) io)) {
					System.err.println("   "+link);
				}
			}
		}
		testForLink("is_right_of", "inverse_of", "is_left_of");
		testForLink("Bob", "is_in_series_with", "Dan");
		testForLink("Dan", "is_right_of", "Bob");
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
		suite.addTest(new InverseAlwaysImpliedTest("testLinks"));
	}

}
