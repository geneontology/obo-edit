package org.oboedit.test;

import java.util.*;

import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.history.HistoryItem;
import org.obo.history.NamespaceHistoryItem;

public class TermNoNamespaceTest extends OperationTest {

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();
		OBOClass oboClass = testUtil.getRandomClass();
		OBOClass resultClass = (OBOClass) oboClass.clone();

		Namespace newNamespace = null;

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
