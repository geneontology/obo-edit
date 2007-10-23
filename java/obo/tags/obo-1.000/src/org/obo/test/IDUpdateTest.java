package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.io.AuditedPrintStream;
import org.obo.identifier.LinkIDResolution;
import org.obo.identifier.LinkIDWarning;
import org.obo.identifier.UnresolvedIDsException;
import org.obo.util.IDUtil;

import junit.framework.Test;
import junit.framework.TestSuite;


public class IDUpdateTest extends AbstractOBOTest {

	protected IDUpdateTest(String name) {
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
		System.out.println("foo");
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
