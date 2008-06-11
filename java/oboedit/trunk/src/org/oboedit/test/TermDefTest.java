package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOClass;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.HistoryItem;

public class TermDefTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermDefTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();
		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();
		String newText = "Changed term definition";
		resultClass.setDefinition(newText);

		HistoryItem item = new DefinitionChangeHistoryItem(oboClass, newText);
		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
