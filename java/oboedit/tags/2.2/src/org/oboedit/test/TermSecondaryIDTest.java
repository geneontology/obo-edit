package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.SecondaryIDHistoryItem;

public class TermSecondaryIDTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermSecondaryIDTest.class);

	@Override
	public Collection<TestBundle> getTestBundles() {
		Collection<TestBundle> testBundles = new LinkedList<TestBundle>();

		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		String newText = TestUtil.getRandomID();

		resultClass.addSecondaryID(newText);

		HistoryItem item = new SecondaryIDHistoryItem(oboClass, newText, false);

		logger.info("setting secondary id to = " + newText
				+ ", newids = " + resultClass.getSecondaryIDs());

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection<ObjectPair> pairs = new LinkedList<ObjectPair>();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
