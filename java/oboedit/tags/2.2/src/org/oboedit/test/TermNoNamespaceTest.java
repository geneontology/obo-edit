package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.NamespaceHistoryItem;

public class TermNoNamespaceTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermNoNamespaceTest.class);

	@Override
	public Collection<TestBundle> getTestBundles() {
		Collection<TestBundle> testBundles = new LinkedList<TestBundle>();
		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		Namespace newNamespace = null;

		resultClass.setNamespace(newNamespace);

		HistoryItem item = new NamespaceHistoryItem(oboClass, newNamespace);
		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection<ObjectPair> pairs = new LinkedList<ObjectPair>();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);

		return testBundles;
	}
}
