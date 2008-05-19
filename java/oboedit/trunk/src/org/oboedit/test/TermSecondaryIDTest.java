package org.oboedit.test;

import java.util.*;

import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.SecondaryIDHistoryItem;

import org.apache.log4j.*;

public class TermSecondaryIDTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermSecondaryIDTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();

		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		String newText = testUtil.getRandomID();

		resultClass.addSecondaryID(newText);

		HistoryItem item = new SecondaryIDHistoryItem(oboClass, newText, false);

		logger.info("setting secondary id to = " + newText
				+ ", newids = " + resultClass.getSecondaryIDs());

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
