package org.obo.nlp.test;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestSuite;

import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.impl.RegulationTermParser;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;


public class RegulationOfSomitogenesisTest extends AbstractNLPTest {

	public RegulationOfSomitogenesisTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "regulation_of_somitogenesis.obo" };
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
			if (report.contains("OK: regulation of transcription, DNA-dependent"))
				passes++;
			if (report.contains("OK: regulation of transcription, DNA-dependent"))
				passes++;
			
		}
		String id = "GO:0014807"; // regulation of somitogenesis
		testForGenus(id,"GO:0065007"); /* asserted */
		testForDifferentium(id,"regulates", "GO:0001756"); /* asserted */
		testForNoIsA(id, "GO:005079"); /* RoDP - asserted, not specific enough */
		
		ReasonerFactory rf = new LinkPileReasonerFactory();
//		ReasonerFactory rf = new ForwardChainingReasonerFactory();
		ReasonedLinkDatabase reasoner = rf.createReasoner();
		reasoner.setLinkDatabase(session.getLinkDatabase());
		reasoner.recache();
		
		// we need transitivity over for this to work
		testForIsA(reasoner, id, "GO:0045995"); /* regulation of embryonic development */
		
	}
	
	
	
	public static void addTests(TestSuite suite) {
		suite.addTest(new RegulationOfSomitogenesisTest("testLinks"));
	}
	

}



