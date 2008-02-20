package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.filters.EqualsComparison;
import org.obo.filters.Filter;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.filters.RegexpComparison;
import org.obo.util.FilterUtil;


public class FilterTest extends AbstractOBOTest {

	LinkFilterFactory lff = new LinkFilterFactory();
	ObjectFilterFactory off = new ObjectFilterFactory();

	public FilterTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "caro.obo", "bone.obo" };
		return Arrays.asList(files);
	}

	public Collection<IdentifiedObject> filterObjects(ObjectFilter filter) {
		Collection<IdentifiedObject> matches = 
			new LinkedList<IdentifiedObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (filter.satisfies(io))
				matches.add(io);
		}
		System.out.println(filter+" N_matches: "+matches.size());
		return matches;
	}

	public Collection<Link> filterLinks(Filter filter) {
		Collection<Link> matches = 
			new LinkedList<Link>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject)io;
				for (Link link : lo.getParents()) {
					if (filter.satisfies(link))
						matches.add(link);
				}
			}
		}
		return matches;
	}


	public void testSearch() throws Exception {
		ObjectFilter filter = (ObjectFilter)off.createNewFilter();
		RegexpComparison c;
		c = new RegexpComparison();

		filter.setComparison(c);
		filter.setValue(".*epithel.*");
		Collection<IdentifiedObject> matches = filterObjects(filter);
		assertTrue(matches.size() > 8);

		filter.setComparison(c);
		filter.setValue("endochondral_bone");
		matches = filterObjects(filter);
		assertTrue(matches.size() == 1);

		filter = (ObjectFilter)off.createNewFilter();
		filter.setCriterion(new IsCompleteLinkCriterion());
		 matches = filterObjects(filter);
		assertTrue(matches.size() == 0);

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
	public void testLinkFilter() throws Exception {
		LinkFilter lfilter = getLinkFilter("CARO:0000003", LinkFilter.PARENT);
		Collection<Link> matches = filterLinks(lfilter);
		System.out.println(lfilter+" N_matches: "+matches.size());
		// note: if caro.obo changes, this may need changing
		assertTrue(matches.size() == 10);
		
		// intersection_of tags
		ObjectFilter ofilter = (ObjectFilter)off.createNewFilter();
		ofilter = (ObjectFilter)off.createNewFilter();
		ofilter.setCriterion(new IsCompleteLinkCriterion());
		lfilter = (LinkFilter)lff.createNewFilter();
		lfilter.setAspect(LinkFilter.SELF);
		lfilter.setFilter(ofilter);
		matches = filterLinks(lfilter);
		System.out.println(lfilter+" N_matches: "+matches.size());

		assertTrue(matches.size() == 2);
		
		// set both child and parent tags


	}

	public void testLinkFilterFindParents() throws Exception {
		LinkFilter lfilter = getLinkFilter("CARO:0000003", LinkFilter.CHILD);
		
		Collection<Link> matches = filterLinks(lfilter);
		System.out.println(lfilter+" N_matches: "+matches.size());
		// note: if caro.obo changes, this may need changing
		assertTrue(matches.size() == 1);

	}
	
	public void testLinkFilterConstrainBoth() throws Exception {
		LinkFilter lfilter1 = getLinkFilter("CARO:0000003", LinkFilter.CHILD);
		LinkFilter lfilter2 = getLinkFilter("CARO:0000006", LinkFilter.PARENT);
		 Filter<Link> filter = FilterUtil.mergeFilters(lfilter1, lfilter2);
		
		Collection<Link> matches = filterLinks(filter);
		System.out.println(filter+" N_matches: "+matches.size());
		// note: if caro.obo changes, this may need changing
		assertTrue(matches.size() == 1);
	}
	
	private LinkFilter getLinkFilter(String id, int aspect) {
		LinkFilter lfilter = (LinkFilter)lff.createNewFilter();
		ObjectFilter ofilter = (ObjectFilter)off.createNewFilter();
		EqualsComparison c = new EqualsComparison();
		ofilter.setComparison(c);
		ofilter.setValue(id);
		lfilter.setFilter(ofilter);
		lfilter.setAspect(aspect);
		return lfilter;
	}


}
