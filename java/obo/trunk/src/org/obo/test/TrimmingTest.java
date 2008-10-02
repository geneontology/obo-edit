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
import org.obo.datamodel.impl.FilteredLinkDatabase;
import org.obo.reasoner.impl.TrimmedLinkDatabase;

import org.apache.log4j.*;

public class TrimmingTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TrimmingTest.class);

	public TrimmingTest(String name) {
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

	protected long oldTime = 0;

	protected long newTime = 0;

	public void testLinks() throws Exception {
		oldTime = 0;
		newTime = 0;
		// Collection<OBOClass> roots = TermUtil.getRoots(reasonedDB);
		// for (OBOClass root : roots) {
		// Iterator<Link> it = reasonedDB.getChildren(root).iterator();
		TrimmedLinkDatabase linkDatabase = new TrimmedLinkDatabase(
				new FilteredLinkDatabase(reasonedDB));
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (!(io instanceof LinkedObject))
				continue;

			long time = System.nanoTime();
			linkDatabase.normalTrimming = false;
			fetchParents((LinkedObject) io, linkDatabase);
			newTime += System.nanoTime() - time;
			time = System.nanoTime();
			linkDatabase.normalTrimming = true;
			fetchParents((LinkedObject) io, linkDatabase);
			oldTime += System.nanoTime() - time;

		}
		logger.info("original implementation time = " + oldTime
				/ 1000000000d);
		System.err
				.println("new implementation time = " + newTime / 1000000000d);
		// }
	}

	protected static int checkedLinkCount = 0;

	protected static void fetchParents(LinkedObject object,
			TrimmedLinkDatabase linkDatabase) {
		Collection<Link> parents = linkDatabase.getParents(object);
		for (Link link : parents) {
			checkedLinkCount++;
		}
		Collection<Link> children = linkDatabase.getChildren(object);
		for (Link link : children) {
			checkedLinkCount++;
		}
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
		suite.addTest(new TrimmingTest("testLinks"));
	}

}
