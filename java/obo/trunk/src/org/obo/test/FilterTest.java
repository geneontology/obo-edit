package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.io.AuditedPrintStream;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.IdentifiedObject;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.filters.RegexpComparison;
import org.obo.filters.SearchComparison;
import org.obo.identifier.LinkIDResolution;
import org.obo.identifier.LinkIDWarning;
import org.obo.identifier.UnresolvedIDsException;
import org.obo.query.QueryEngine;
import org.obo.query.impl.FilterQuery;
import org.obo.query.impl.SearchHit;
import org.obo.util.IDUtil;


import junit.framework.Test;
import junit.framework.TestSuite;


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
