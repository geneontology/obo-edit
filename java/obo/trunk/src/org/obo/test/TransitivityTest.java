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

import org.apache.log4j.*;

public class TransitivityTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TransitivityTest.class);

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
				logger.info("parents of "+io);
				Collection<Link> parents = reasonedDB.getParents((LinkedObject) io); 
				for(Link link : parents) {
					logger.info("   "+link);
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
		logger.info("foo");
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
