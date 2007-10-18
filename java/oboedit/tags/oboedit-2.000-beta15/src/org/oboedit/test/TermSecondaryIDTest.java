package org.oboedit.test;

import java.util.*;

import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.SecondaryIDHistoryItem;

public class TermSecondaryIDTest extends OperationTest {

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();

		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		String newText = testUtil.getRandomID();

		resultClass.addSecondaryID(newText);

		HistoryItem item = new SecondaryIDHistoryItem(oboClass, newText, false);

		System.err.println("setting secondary id to = " + newText
				+ ", newids = " + resultClass.getSecondaryIDs());

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
