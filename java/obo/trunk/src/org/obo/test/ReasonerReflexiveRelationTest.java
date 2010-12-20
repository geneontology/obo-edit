package org.obo.test;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.filters.SubsetSearchCriterion;
import org.obo.filters.EqualsComparison;
import org.obo.filters.Filter;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.IsImpliedLinkCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.filters.RegexpComparison;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.reasoner.rbr.RuleBasedReasonerFactory;
import org.obo.util.FilterUtil;


import org.apache.log4j.*;

public class ReasonerReflexiveRelationTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerReflexiveRelationTest.class);

	public ReasonerReflexiveRelationTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "carx.obo" };
		return Arrays.asList(files);
	}
	/*
	protected ReasonedLinkDatabase createReasoner() {
		reasonerFactory =
			new LinkPileReasonerFactory();

		return reasonerFactory.createReasoner();
	}
	 */
	
	public void testFilter() {
		boolean hasReflexive = false;
		Collection<Link> rlinks = reasonedDB.getLinks(OBOProperty.IS_A);
		for (Link link : rlinks) {
			System.out.println("REASONER:"+link);
			if (link.getChild().equals(link.getParent()))
				hasReflexive = true;
		}
		TrimmedLinkDatabase tdb = new TrimmedLinkDatabase(reasonedDB);
		Collection<Link> links = tdb.getLinks(OBOProperty.IS_A);
		for (Link link : links) {
			System.out.println("TDB:"+link);
			if (link.getChild().equals(link.getParent()))
				hasReflexive = true;
		}
		// the reasoner should not provide reflexive links;
		// this would confuse the current display model
		assertTrue(!hasReflexive);
		
		LinkedObject car = (LinkedObject)session.getObject("TEST:0000001");
		for (Link link : reasonedDB.getChildren(car)) {
			System.out.println("RDB-children:"+link);
			if (link.getChild().equals(car))
				hasReflexive = true;
		}
		for (Link link : tdb.getChildren(car)) {
			System.out.println("TDB-children:"+link);
			if (link.getChild().equals(car))
				hasReflexive = true;
		}
		assertTrue(!hasReflexive);
	}



}
