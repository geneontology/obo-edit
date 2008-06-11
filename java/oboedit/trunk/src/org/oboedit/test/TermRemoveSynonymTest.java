package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.history.DelSynonymHistoryItem;
import org.obo.history.HistoryItem;

public class TermRemoveSynonymTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermRemoveSynonymTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();

		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		String newText = testUtil.getRandomString(20);

		Synonym syn = new SynonymImpl(newText);

		oboClass.addSynonym(syn);

		HistoryItem item = new DelSynonymHistoryItem(oboClass, newText);

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
