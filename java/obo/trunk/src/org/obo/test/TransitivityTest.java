package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasoner;
import org.obo.reasoner.impl.PelletWrappedReasonerFactory;

import junit.framework.Test;
import junit.framework.TestSuite;

public class TransitivityTest extends AbstractReasonerTest {

	public TransitivityTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "inverse_always_implied.obo" ,
				"part_of_test.obo"};
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
				Collection<Link> parents = reasonedDB.getParents((LinkedObject) io); 
				for(Link link : parents) {
					System.err.println("   "+link);
				}
			}
		}
		
		testForLink("Z:4","part_of","X:0");
		// these examples are somehwat artificial as "Bob" and "Jane"
		// denote classes
		testForLink("Bob", "is_in_series_with", "Dan");
		testForLink("Bob", "is_left_of", "Emily");
		
		// test always implies inverse link. TODO - all reasoners?
		// testForLink("Emily", "is_right_of", "Bob");
		
		/*
		 * make sure reasoner has both asserted inverse link and
		 * the implied link through symmetry.
		 *  Can/should pellet do this?
		 */
		testForLink("is_left_of", "inverse_of", "is_right_of");
		if (!getReasonerFactory().getClass().equals(PelletWrappedReasonerFactory.class))
			testForLink("is_right_of", "inverse_of", "is_left_of");
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
