package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.SecondaryIDHistoryItem;

public class TermRemoveSecondaryIDTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermRemoveSecondaryIDTest.class);

	@Override
	public Collection<TestBundle> getTestBundles() {
		Collection<TestBundle> testBundles = new LinkedList<TestBundle>();

		OBOClass oboClass = testUtil.getRandomClass();

		String newText = TestUtil.getRandomID();
		oboClass.addSecondaryID(newText);

		OBOClass resultClass = (OBOClass) oboClass.clone();
		resultClass.removeSecondaryID(newText);

		HistoryItem item = new SecondaryIDHistoryItem(oboClass, newText, true);

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection<ObjectPair> pairs = new LinkedList<ObjectPair>();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
