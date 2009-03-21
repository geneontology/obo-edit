package org.obo.test;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.filters.SubsetSearchCriterion;
import org.obo.filters.EqualsComparison;
import org.obo.filters.Filter;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.IsRedundantLinkCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.filters.RegexpComparison;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasoner;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.util.FilterUtil;
import org.obo.util.TermUtil;


import org.apache.log4j.*;

public class RedundancyTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RedundancyTest.class);

	LinkFilterFactory lff = new LinkFilterFactory();
	ObjectFilterFactory off = new ObjectFilterFactory();


	public RedundancyTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "redundant1.obo" };
		return Arrays.asList(files);
	}



	public void testIsRedundant() throws Exception {

		for (OBOObject oo : TermUtil.getOBOObjects(session)) {
			for (Link link : reasonedDB.getParents(oo)) {
				System.out.println(link.getClass()+" "+link);
				if (reasonedDB instanceof LinkPileReasoner) {
					LinkPileReasoner lpr = (LinkPileReasoner) reasonedDB;
					Link rl = lpr.findRealLink(link);
					System.out.println(" RL:"+rl.getClass()+" "+rl);
				}
				Collection<Explanation> exps = reasonedDB.getExplanations(link);
				for (Explanation exp : exps) {
					System.out.println("EXP: "+exp);
				}
			}
		}
		testForIsA("C","A");
		testForRedundantIsA("C","A");
		
		ObjectFilter ofilter = (ObjectFilter)off.createNewFilter();
		ofilter = (ObjectFilter)off.createNewFilter();
		ofilter.setCriterion(new IsRedundantLinkCriterion());
		LinkFilter lfilter = (LinkFilter)lff.createNewFilter();
		lfilter.setAspect(LinkFilter.SELF);
		lfilter.setFilter(ofilter);

		Collection<Link> matches = filterLinks(lfilter);
		logger.info(lfilter+" N_matches: "+matches.size());
		System.out.println(lfilter+" N_matches: "+matches.size());
		// TODO
		//assertTrue(matches.size() == 1);
		
		// set both child and parent tags
	}
	


}
