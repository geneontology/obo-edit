package org.obo.nlp.test;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestSuite;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.impl.RegulationTermParser;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;


public class PositiveRegulationTermParserTest extends AbstractNLPTest {

	public PositiveRegulationTermParserTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "positive_regulation.obo" };
		return Arrays.asList(files);
	}
	
	
	public void testLinks() throws Exception {
		semanticParser = new RegulationTermParser();
		semanticParser.index(session);
		Collection<TermMacroHistoryItem> items = semanticParser.parseTerms();
		for (TermMacroHistoryItem item : items) {
			System.out.println(item);
		}
		semanticParser.apply(items);
		int passes = 0;
		for (String report : semanticParser.getReports()) {
			System.out.println(report);
			if (report.contains("HIERARCHY: GO:0048518"))
				passes++;
			if (report.contains("OK: GO:0051046"))
				passes++;
			
		}
		assertTrue(passes == 2);

		String id = "GO:0048518"; //  positive regulation of biological process
		testForGenus(id,"GO:0065007"); /* BR: asserted */
		testForDifferentium(id,"positively_regulates", "GO:0008150"); /* asserted */
		
		id = "GO:0046887"; // positive regulation of hormone secretion
		testForGenus(id, "GO:0065007"); /* BR: asserted */
				
		ReasonerFactory rf = new LinkPileReasonerFactory();
//		ReasonerFactory rf = new ForwardChainingReasonerFactory();
		ReasonedLinkDatabase reasoner = rf.createReasoner();
		reasoner.setLinkDatabase(session.getLinkDatabase());
		reasoner.recache();

		testForIsA(reasoner, id, "GO:0046883");  // regulation of hormone secretion
		testForIsA(reasoner, id, "GO:0048522");   // positive regulation of cellular process
		testForIsA(reasoner, id, "GO:0051047"); // positive regulation of secretion
	
	}
	
	
	

	
	public static void addTests(TestSuite suite) {
		suite.addTest(new PositiveRegulationTermParserTest("testLinks"));
	}
	

}



