package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.Link;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import junit.framework.Test;
import junit.framework.TestSuite;

public class TrimmingLibraryTest extends AbstractReasonerTest {

	protected TrimmingLibraryTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "so-xp.obo" };
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}

	protected boolean testLink(Link link) throws Exception {
		long time = System.nanoTime();
		boolean firstResult = ReasonerUtil.shouldBeTrimmedOld(reasonedDB, link);
		oldTime += System.nanoTime() - time;
		time = System.nanoTime();
		boolean secondResult = ReasonerUtil.shouldBeTrimmed(reasonedDB, link);
		newTime += System.nanoTime() - time;
		boolean match = firstResult == secondResult;
		if (!match) {
			System.err.println("testing link: " + link);
			System.err.println("    original implementation (" + firstResult
					+ ") = " + oldTime);
			System.err.println("    new implementation (" + secondResult
					+ ") = " + newTime);
			System.err.println();
		}
		return match;
	}

	protected long oldTime = 0;

	protected long newTime = 0;

	public void testLinks() throws Exception {
		oldTime = 0;
		newTime = 0;
		// Collection<OBOClass> roots = TermUtil.getRoots(reasonedDB);
		// for (OBOClass root : roots) {
		// Iterator<Link> it = reasonedDB.getChildren(root).iterator();
		Iterator<Link> it = TermUtil.getAllLinks(reasonedDB);
		while (it.hasNext()) {
			Link link = it.next();
			if (TermUtil.isIntersection(link))
				continue;
			boolean match = testLink(link);
			assertTrue(match);
		}
		System.err.println("original implementation time = "+oldTime);
		System.err.println("new implementation time = "+newTime);
		// }
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
		suite.addTest(new TrimmingLibraryTest("testLinks"));
	}

}
