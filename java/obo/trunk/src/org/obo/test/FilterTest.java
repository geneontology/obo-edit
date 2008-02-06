package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.filters.RegexpComparison;


public class FilterTest extends AbstractOBOTest {

	public FilterTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "caro.obo" };
		return Arrays.asList(files);
	}

	public static Collection<String> getTests() {
		String[] tests = {};
		return Arrays.asList(tests);
	}

	public void testSearch() throws Exception {
		ObjectFilterFactory off = new ObjectFilterFactory();
		ObjectFilter filter = (ObjectFilter)off.createNewFilter();
		RegexpComparison c;
		c = new RegexpComparison();
		
		filter.setComparison(c);
		filter.setValue(".*epithel.*");
		Collection<IdentifiedObject> matches = 
			new LinkedList<IdentifiedObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (filter.satisfies(io))
				matches.add(io);
		}
		System.out.println(filter+" N_matches: "+matches.size());
		assertTrue(matches.size() > 8);
		/*
		FilterQuery fq = 
			new FilterQuery(filter, 
				null,
				null);
		QueryEngine engine = new QueryEngine(session);
		TaskDelegate<Collection<SearchHit<?>>>  r = engine.query(fq);
		System.out.println(r.getResults().size());
		
		
		
		*/
		
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
		suite.addTest(new FilterTest("testSearch"));
	}

}
