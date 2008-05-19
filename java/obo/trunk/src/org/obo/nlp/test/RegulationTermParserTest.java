package org.obo.nlp.test;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestSuite;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.Namer;
import org.obo.nlp.impl.RegulationTermParser;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;


import org.apache.log4j.*;

public class RegulationTermParserTest extends AbstractNLPTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RegulationTermParserTest.class);

	public RegulationTermParserTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "regulation_of_transcription.obo" };
			return Arrays.asList(files);
	}
	
	public void testLinks() throws Exception {
		semanticParser = new RegulationTermParser();
		semanticParser.index(session);
		Collection<TermMacroHistoryItem> items = semanticParser.parseTerms();
		for (TermMacroHistoryItem item : items) {
			logger.info(item);
		}
		semanticParser.apply(items);
		int passes = 0;
		for (String report : semanticParser.getReports()) {
			logger.info(report);
			if (report.contains("MISSING_LINK: GO:0019219"))
				passes++;
			if (report.contains("NO_TARGET: GO:0021882"))
				passes++;
			
		}
		assertTrue(passes == 2);
		OBOProperty nrProp = (OBOProperty) session.getObject("negatively_regulates");
		testForIsA("negatively_regulates","regulates");
		testForTransitiveOver("negatively_regulates","part_of");
		String id = "GO:0031323";
		testForGenus(id,"GO:0065007"); /* asserted */
		testForDifferentium(id,"regulates", "GO:0044237"); /* asserted */
		testForNoIsA(id, "GO:0019222"); /* RoMP */
		
		ReasonerFactory rf = new LinkPileReasonerFactory();
//		ReasonerFactory rf = new ForwardChainingReasonerFactory();
		ReasonedLinkDatabase reasoner = rf.createReasoner();
		reasoner.setLinkDatabase(session.getLinkDatabase());
		reasoner.recache();
		
		testForIsA(reasoner, id, "GO:0019222"); /* RoMP */
		
		// transitive-over test
		testForIsA(reasoner,"GO:0045449","GO:0010468");
		
		/* 
		 * synonyms
		 */
		semanticParser.useDefaultNamer();
		Namer namer = semanticParser.getNamer();
		id = "GO:0019222";
		LinkedObject lo = (LinkedObject)session.getObject(id);
		Collection<String> names = namer.constructNames(lo );
		logger.info(names);
		assertTrue(names.contains("regulation of metabolism"));
		assertTrue(names.size() == 1);
		
	}
	

	
	public static void addTests(TestSuite suite) {
		suite.addTest(new RegulationTermParserTest("testLinks"));
	}
	

}



