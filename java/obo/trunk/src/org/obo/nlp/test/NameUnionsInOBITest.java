package org.obo.nlp.test;

import java.util.Arrays;
import java.util.Collection;

import junit.framework.TestSuite;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.Namer;
import org.obo.nlp.NamerUtil;
import org.obo.nlp.impl.LogicalDefinitionNamer;
import org.obo.nlp.impl.RegulationTermParser;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.util.TermUtil;


import org.apache.log4j.*;

public class NameUnionsInOBITest extends AbstractNLPTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NameUnionsInOBITest.class);

	public NameUnionsInOBITest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files = {  "http://purl.org/obo/obo-all/obi/obi.obo" };
			return Arrays.asList(files);
	}
	
	public void testNaming() throws Exception {
		
		
		LogicalDefinitionNamer namer = new LogicalDefinitionNamer();
		String id = "obi:OBI_0100016";
		LinkedObject lo = (LinkedObject)session.getObject(id);
		boolean ok = false;
		Collection<String> names = namer.constructNames(lo );
		for (Link link : lo.getParents()) {
			LinkedObject po = link.getParent();
			if (po.getName() != null)
				continue;
			names.addAll(namer.constructNames(po ));
			logger.info("names="+names);
			for (String name : names) {
				logger.info("n="+name);
				if (name.contains("reagent") && name.contains("specimen"))
					ok = true;
			}
		}
		assertTrue(ok);
		assertTrue(names.size() == 1);

		IdentifiedObject foo = session.getObject("obi:OBI_0000305");
		System.out.println(foo);
		System.out.println(foo.getName());
		
		assertTrue(foo.getName().equals("datum"));
		
		
		Collection<HistoryItem> items = NamerUtil.getNameUnnamedObjectsAction(session, namer);
		for (HistoryItem item : items) {
			System.out.println(item);
		}

	}
	

	
	public static void addTests(TestSuite suite) {
		suite.addTest(new NameUnionsInOBITest("testLinks"));
	}
	

}



