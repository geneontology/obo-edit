package org.oboedit.test;

import java.util.*;

import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.NamespaceHistoryItem;

import org.apache.log4j.*;

public class TermNamespaceTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermNamespaceTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();
		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		Namespace newNamespace = testUtil.getRandomNamespace(oboClass
				.getNamespace());

		resultClass.setNamespace(newNamespace);

		HistoryItem item = new NamespaceHistoryItem(oboClass, newNamespace);
		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);

		return testBundles;
	}
}
