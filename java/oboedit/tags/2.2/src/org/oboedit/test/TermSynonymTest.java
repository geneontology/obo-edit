package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.history.AddSynonymHistoryItem;
import org.obo.history.HistoryItem;

public class TermSynonymTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermSynonymTest.class);

	@Override
	public Collection<TestBundle> getTestBundles() {
		Collection<TestBundle> testBundles = new LinkedList<TestBundle>();

		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		String newText = TestUtil.getRandomString(20);

		Synonym syn = new SynonymImpl(newText);

		resultClass.addSynonym(syn);

		HistoryItem item = new AddSynonymHistoryItem(oboClass, newText);

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection<ObjectPair> pairs = new LinkedList<ObjectPair>();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
