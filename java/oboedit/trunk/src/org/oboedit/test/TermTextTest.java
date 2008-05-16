package org.oboedit.test;

import java.util.*;

import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;

import org.apache.log4j.*;

public class TermTextTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermTextTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();
		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();
		String newText = "Changed term name";
		resultClass.setName(newText);

		HistoryItem item = new NameChangeHistoryItem(oboClass, newText);
		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
