package org.obo.test;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.filters.CategorySearchCriterion;
import org.obo.filters.EqualsComparison;
import org.obo.filters.Filter;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.IsRedundantLinkCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.filters.RegexpComparison;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.impl.ForwardChainingReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasoner;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.util.FilterUtil;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;


import org.apache.log4j.*;

public class RemoveRedundantLinksTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RemoveRedundantLinksTest.class);

	LinkFilterFactory lff = new LinkFilterFactory();
	ObjectFilterFactory off = new ObjectFilterFactory();


	public RemoveRedundantLinksTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = { "regulation_of_anti_apoptosis_xp.obo" };
		return Arrays.asList(files);
	}


	
	public void testRemoveRedundant() throws Exception {
		Iterator<Link> it;
		
		Collection<Link> rls = ReasonerUtil.getAllRedundantLinks(reasonedDB, session.getLinkDatabase());
		logger.info("num redundant links: "+rls.size());
		for (Link rl : rls) {
			logger.info("redundant: "+rl+" "+TermUtil.isIntersection(rl));
		}
		assertTrue(rls.size() > 40);
		
		Set<Link> ilinks = new HashSet<Link>();
		it = TermUtil.getAllLinks(session.getLinkDatabase());
		while (it.hasNext()) {
			Link link = it.next();
			if (TermUtil.isIntersection(link)) {
				ilinks.add(link);
			}	
		}
		assertTrue(ilinks.size() > 30);

		
		TermMacroHistoryItem item = new TermMacroHistoryItem(
				"Delete "+rls.size()+" redundant links");
		for (Link link : rls) {
			item.addItem(new DeleteLinkHistoryItem(link));
		}
		session.getOperationModel().apply(item);
		it = TermUtil.getAllLinks(session.getLinkDatabase());
		while (it.hasNext()) {
			Link link = it.next();
			logger.info("remaining: "+link+" "+TermUtil.isIntersection(link));
		}
		
		logger.info("checking that no intersection links disappeared; num="+ilinks.size());
		boolean pass = true;
		for (Link ilink : ilinks) {
			boolean found = false;
			for (Link elink : session.getLinkDatabase().getParents(ilink.getChild())) {
				if (elink.equals(ilink)) { 
					if (TermUtil.isIntersection(elink)) {
						logger.info("found: "+ilink);
						found = true;
						break;
					}
					else {
						logger.info("replaced by non-intersection?: "+elink);
					}
				}
			}
			if (!found) {
				logger.error("cannot find: "+ilink);
				pass = false;
			}
			assertTrue(pass);
		}
		
		logger.info("checking that all redundant links have disappeared; num="+rls.size());
		pass = true;
		for (Link rl : rls) {
			boolean found = false;
			for (Link elink : session.getLinkDatabase().getParents(rl.getChild())) {
				if (elink.equals(rl)) { 
					found = true;
					break;
				}
			}
			if (found) {
				logger.error("failed to remove: "+rl);
				pass = false;
			}
			else {
				logger.info("successfully removed: "+rl);
			}
			assertTrue(pass);
		}
		
		

	}


}
