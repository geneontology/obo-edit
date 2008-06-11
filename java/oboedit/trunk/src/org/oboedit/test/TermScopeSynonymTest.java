package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymCategory;
import org.obo.datamodel.impl.SynonymCategoryImpl;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.history.ChangeSynScopeHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.util.HistoryUtil;

public class TermScopeSynonymTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermScopeSynonymTest.class);

	@Override
	public Collection getTestBundles() {
		Collection testBundles = new LinkedList();

		OBOClass oboClass = testUtil.getRandomClass();

		String newText = testUtil.getRandomString(20);

		SynonymCategory synCat = new SynonymCategoryImpl();

		Synonym syn = new SynonymImpl(newText);

		oboClass.addSynonym(syn);

		OBOClass resultClass = (OBOClass) oboClass.clone();
		Synonym resultSyn = HistoryUtil.findSynonym(resultClass, newText);
		resultSyn.setScope(Synonym.EXACT_SYNONYM);

		HistoryItem item = new ChangeSynScopeHistoryItem(oboClass, syn,
				Synonym.EXACT_SYNONYM);

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection pairs = new LinkedList();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
