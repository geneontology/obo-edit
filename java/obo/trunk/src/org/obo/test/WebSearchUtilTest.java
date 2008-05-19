package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.util.WebSearchUtil;
import org.obo.util.WebSearchUtil.SearchableDatabase;


import org.apache.log4j.*;

public class WebSearchUtilTest extends AbstractOBOTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(WebSearchUtilTest.class);

	public WebSearchUtilTest(String name) {
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

	public void testExpand() throws Exception {
		Set<String> terms = WebSearchUtil.expandSearchTerm(session, "transcript");
		for (String term : terms)
			logger.info(term);
		assertTrue(terms.size() > 0);
		String qs = WebSearchUtil.createQueryString(SearchableDatabase.NCBI, terms);
		logger.info(qs);
		assertTrue(qs.contains("RNA"));
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
		suite.addTest(new WebSearchUtilTest("testExpand"));
	}

}
