package org.oboedit.test;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.Logger;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymType;
import org.obo.datamodel.impl.SynonymTypeImpl;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.history.ChangeSynScopeHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.util.HistoryUtil;

public class TermScopeSynonymTest extends OperationTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermScopeSynonymTest.class);

	@Override
	public Collection<TestBundle> getTestBundles() {
		Collection<TestBundle> testBundles = new LinkedList<TestBundle>();

		OBOClass oboClass = testUtil.getRandomClass();

		String newText = TestUtil.getRandomString(20);

		SynonymType synCat = new SynonymTypeImpl();

		Synonym syn = new SynonymImpl(newText);

		oboClass.addSynonym(syn);

		OBOClass resultClass = (OBOClass) oboClass.clone();
		Synonym resultSyn = HistoryUtil.findSynonym(resultClass, newText);
		resultSyn.setScope(Synonym.EXACT_SYNONYM);

		HistoryItem item = new ChangeSynScopeHistoryItem(oboClass, syn,
				Synonym.EXACT_SYNONYM);

		ObjectPair pair = new ObjectPair(oboClass, resultClass);
		Collection<ObjectPair> pairs = new LinkedList<ObjectPair>();
		pairs.add(pair);
		TestBundle out = new TestBundle(item, pairs);
		testBundles.add(out);
		return testBundles;
	}
}
