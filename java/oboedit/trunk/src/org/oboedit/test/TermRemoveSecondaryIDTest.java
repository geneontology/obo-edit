package org.oboedit.test;

import java.util.*;

import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.SecondaryIDHistoryItem;

import org.apache.log4j.*;

public class TermRemoveSecondaryIDTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermRemoveSecondaryIDTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();

		OBOClass oboClass = testUtil.getRandomClass();

		String newText = testUtil.getRandomID();
		oboClass.addSecondaryID(newText);

		OBOClass resultClass = (OBOClass) oboClass.clone();
		resultClass.removeSecondaryID(newText);

		HistoryItem item = new SecondaryIDHistoryItem(oboClass, newText, true);

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
