package org.oboedit.test;

import java.util.*;

import org.obo.datamodel.OBOClass;
import org.obo.history.CommentChangeHistoryItem;
import org.obo.history.HistoryItem;

import org.apache.log4j.*;

public class TermCommentTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermCommentTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();
		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();
		String newText = testUtil.getRandomString(40);
		resultClass.setComment(newText);

		HistoryItem item = new CommentChangeHistoryItem(oboClass, newText);
		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
