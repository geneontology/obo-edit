package org.obo.test;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.bbop.io.AuditedPrintStream;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.util.TermUtil;
import org.obo.datamodel.*;
import org.obo.filters.IsCompleteLinkCriterion;
import org.obo.filters.IsImpliedLinkCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterFactory;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;

import org.apache.log4j.*;

public class IntersectionCamphorCatabolismExampleTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionCamphorCatabolismExampleTest.class);

	public IntersectionCamphorCatabolismExampleTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"camphor_catabolism.obo"};
		return Arrays.asList(files);
	}



	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testReasonedLinks() throws Exception {
		String CAMPHOR_CATABOLISM = "GO:0019383";
		String XENOBIOTIC_CATABOLISM = "GO:0042178";
		String RCAMPHOR = "CHEBI:15396";
		String CAMPHOR = "CHEBI:36773";
		String CAMPHORS = "CHEBI:22996";
		String XENOBIOTIC = "CHEBI:35703";
		
		LinkedObject camcat = (LinkedObject) session.getObject(CAMPHOR_CATABOLISM);
		for (Link link :reasonedDB.getParents(camcat)) {
			if (link.getType().equals(OBOProperty.IS_A))
				logger.info("  reasonedDB:"+link);
		}
		for (Link link :trimmedDB.getParents(camcat)) {
			if (link.getType().equals(OBOProperty.IS_A))
				logger.info("  trimmedDB:"+link);
		}

		testForIsA(RCAMPHOR, CAMPHOR);
		testForIsA("CHEBI:24974","CHEBI:23367"); /* is_a transitivity */
		testForIsA("CHEBI:33304","CHEBI:33675"); /* asserted */
		testForIsA(CAMPHOR_CATABOLISM,"GO:0009056"); /* genus */
		
		// we expect the reasoner to contain both the XP link and
		// the trivially inferred plain link
		boolean gotXPLink = false;
		boolean gotPlainLink = false;
		for (Link link : getLinks(CAMPHOR_CATABOLISM,OBOProperty.IS_A.getID(),"GO:0009056")) {
			logger.info("genus link: "+link);
			if (TermUtil.isIntersection(link))
				gotXPLink = true;
			else
				gotPlainLink = true;
		}
		assertTrue(gotXPLink);
		assertTrue(gotPlainLink);
		
		testForIsA(CAMPHOR_CATABOLISM,XENOBIOTIC_CATABOLISM); /* completeness : c-catab is_a xeno catab*/
		testForLink("testA","part_of","testB"); /* asserted */
		testForLink("testA","part_of","testC"); /* transitivity */
		testForLink("GO:0019383","UCDHSC:results_in_division_of","CHEBI:35703"); /* differentia + transitivity */

		testForIsAInTrimmed(CAMPHOR_CATABOLISM,XENOBIOTIC_CATABOLISM); 
		testForLink("GO:0019383","UCDHSC:results_in_division_of","CHEBI:35703"); /* differentia + transitivity */

		// test incremental reasoning
		if (true) {
			Link link = reasonedDB.hasRelationship((LinkedObject)session.getObject(RCAMPHOR), OBOProperty.IS_A, (LinkedObject)session.getObject(CAMPHOR));
			// remove a link that is crucial for inferencing
			HistoryItem item = new DeleteLinkHistoryItem(link);
			session.getOperationModel().apply(item);
			reasonedDB.removeLink(link);
			testForNoIsA(CAMPHOR_CATABOLISM,XENOBIOTIC_CATABOLISM); /* completeness : c-catab is_a xeno catab*/
			// put the link back
			item = new CreateLinkHistoryItem(link);
			session.getOperationModel().apply(item);
			reasonedDB.addLink(link);
			testForIsA(CAMPHOR_CATABOLISM,XENOBIOTIC_CATABOLISM); /* completeness : c-catab is_a xeno catab*/
		}

		// test link filter
		if (true) {
			LinkFilterFactory lff = new LinkFilterFactory();
			ObjectFilterFactory off = new ObjectFilterFactory();

			LinkFilter lfilter = (LinkFilter)lff.createNewFilter();
			// intersection_of tags
			ObjectFilter ofilter = (ObjectFilter)off.createNewFilter();
			ofilter = (ObjectFilter)off.createNewFilter();
			ofilter.setCriterion(new IsImpliedLinkCriterion());
			lfilter = (LinkFilter)lff.createNewFilter();
			lfilter.setAspect(LinkFilter.SELF);
			lfilter.setFilter(ofilter);
			Collection<Link> matches = filterReasonedLinks(lfilter);
			logger.info(lfilter+" N_matches: "+matches.size());

			assertTrue(matches.size() > 0);
		}
		writeTempTrimmedReasonedOBOFile();
	}
}

