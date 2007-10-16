package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.LinkPileReasoner;

import junit.framework.Test;
import junit.framework.TestSuite;

public class TransitivityTest extends AbstractReasonerTest {

	protected TransitivityTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "inverse_always_implied.obo" };
		return Arrays.asList(files);
	}
	
	@Override
	protected ReasonedLinkDatabase createReasoner() {
		LinkPileReasoner reasoner = new LinkPileReasoner();
		return reasoner;
	}

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		for(IdentifiedObject io : reasonedDB.getObjects()) {
			if (io instanceof LinkedObject) {
				System.err.println("parents of "+io);
				Collection<Link> parents = reasonedDB.getParents((LinkedObject) io); 
				for(Link link : parents) {
					System.err.println("   "+link);
				}
			}
		}
		testForLink("is_left_of", "inverse_of", "is_right_of");
		testForLink("is_right_of", "inverse_of", "is_left_of");
		testForLink("Bob", "is_in_series_with", "Dan");
		testForLink("Bob", "is_left_of", "Emily");
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
		suite.addTest(new TransitivityTest("testLinks"));
	}
}
