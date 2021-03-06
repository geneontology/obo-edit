package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.identifier.LinkIDResolution;
import org.obo.identifier.LinkIDWarning;
import org.obo.identifier.UnresolvedIDsException;
import org.obo.util.IDUtil;


import org.apache.log4j.*;

public class IDUpdateTest extends AbstractOBOTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDUpdateTest.class);

	public IDUpdateTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "id_updates.obo" };
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}

	public void testLinks() throws Exception {
		Collection<LinkIDResolution> resolutions = new LinkedList<LinkIDResolution>();
		while (true) {
			try {
				IDUtil.updateIDs(session, resolutions, true);
				break;
			} catch (UnresolvedIDsException ex) {
				for(LinkIDWarning warning : ex.getWarnings()) {
					resolutions.addAll(warning.getResolutions());
				}
			}
		}
		testForIsA("A", "D");
		testForIsA("A", "E");
		testForIsA("A", "Z");
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
		suite.addTest(new IDUpdateTest("testLinks"));
	}

}
