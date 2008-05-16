package org.oboedit.test;

import junit.framework.*;

import org.oboedit.controller.IDManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.oboedit.gui.*;
import org.oboedit.gui.actions.*;
import org.obo.history.HistoryItem;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDProfile;
import org.obo.util.TermUtil;
import org.bbop.util.*;
import java.util.*;
import javax.swing.tree.TreePath;

import org.apache.log4j.*;

public class AddActionTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AddActionTest.class);

	protected Set<String> expectedIDs = new HashSet<String>();

	{
		expectedIDs.add("GO:0100031");
		expectedIDs.add("GO:0000425");
		expectedIDs.add("GO:0000407");
		expectedIDs.add("GO:0100000");
		expectedIDs.add("GO:0000439");
		expectedIDs.add("GO:0000403");
		expectedIDs.add("GO:0100013");
		expectedIDs.add("GO:0100035");
		expectedIDs.add("GO:0000429");
		expectedIDs.add("GO:0100022");
		expectedIDs.add("GO:0000416");
		expectedIDs.add("GO:0000402");
		expectedIDs.add("GO:0100048");
		expectedIDs.add("GO:0000399");
		expectedIDs.add("GO:0100026");
		expectedIDs.add("GO:0000420");
		expectedIDs.add("GO:0000411");
		expectedIDs.add("GO:0100007");
		expectedIDs.add("GO:0000443");
		expectedIDs.add("GO:0100044");
		expectedIDs.add("GO:0100016");
		expectedIDs.add("GO:0000434");
		expectedIDs.add("GO:0100004");
		expectedIDs.add("GO:0000424");
		expectedIDs.add("GO:0000426");
		expectedIDs.add("GO:0000408");
		expectedIDs.add("GO:0000436");
		expectedIDs.add("GO:0100001");
		expectedIDs.add("GO:0100010");
		expectedIDs.add("GO:0000417");
		expectedIDs.add("GO:0100023");
		expectedIDs.add("GO:0100032");
		expectedIDs.add("GO:0100041");
		expectedIDs.add("GO:0000404");
		expectedIDs.add("GO:0100027");
		expectedIDs.add("GO:0000412");
		expectedIDs.add("GO:0100039");
		expectedIDs.add("GO:0000440");
		expectedIDs.add("GO:0100045");
		expectedIDs.add("GO:0000000");
		expectedIDs.add("GO:0100036");
		expectedIDs.add("GO:0000430");
		expectedIDs.add("GO:0100008");
		expectedIDs.add("GO:0000444");
		expectedIDs.add("GO:0000421");
		expectedIDs.add("GO:0100017");
		expectedIDs.add("GO:0100014");
		expectedIDs.add("GO:0000409");
		expectedIDs.add("GO:0000435");
		expectedIDs.add("GO:0000437");
		expectedIDs.add("GO:0100002");
		expectedIDs.add("GO:0000405");
		expectedIDs.add("GO:0100015");
		expectedIDs.add("GO:0100033");
		expectedIDs.add("GO:0000427");
		expectedIDs.add("GO:0100042");
		expectedIDs.add("GO:0100024");
		expectedIDs.add("GO:0000414");
		expectedIDs.add("GO:0100011");
		expectedIDs.add("GO:0100037");
		expectedIDs.add("GO:0000418");
		expectedIDs.add("GO:0100020");
		expectedIDs.add("GO:0100028");
		expectedIDs.add("GO:0000400");
		expectedIDs.add("GO:0000413");
		expectedIDs.add("GO:0100009");
		expectedIDs.add("GO:0100049");
		expectedIDs.add("GO:0100018");
		expectedIDs.add("GO:0000441");
		expectedIDs.add("GO:0000431");
		expectedIDs.add("GO:0000432");
		expectedIDs.add("GO:0100005");
		expectedIDs.add("GO:0000422");
		expectedIDs.add("GO:0000445");
		expectedIDs.add("GO:0100046");
		expectedIDs.add("GO:0100012");
		expectedIDs.add("GO:0000447");
		expectedIDs.add("GO:0000415");
		expectedIDs.add("GO:0100040");
		expectedIDs.add("GO:0100025");
		expectedIDs.add("GO:0100030");
		expectedIDs.add("GO:0000406");
		expectedIDs.add("GO:0000428");
		expectedIDs.add("GO:0000438");
		expectedIDs.add("GO:0100021");
		expectedIDs.add("GO:0100029");
		expectedIDs.add("GO:0000401");
		expectedIDs.add("GO:0100034");
		expectedIDs.add("GO:0000419");
		expectedIDs.add("GO:0100038");
		expectedIDs.add("GO:0000410");
		expectedIDs.add("GO:0100047");
		expectedIDs.add("GO:0000442");
		expectedIDs.add("GO:0100006");
		expectedIDs.add("GO:0100019");
		expectedIDs.add("GO:0000433");
		expectedIDs.add("GO:0100043");
		expectedIDs.add("GO:0000446");
		expectedIDs.add("GO:0000423");
		expectedIDs.add("GO:0100003");
	};

	public void testAddTerms() {
		Iterator it;
		TestUtil testUtil = TestUtil.getInstance();

		// copy the original set of terms
		Set<LinkedObject> oldTerms = new HashSet<LinkedObject>();
		oldTerms.addAll(TermUtil.getTerms(testUtil.getSession()));

		// set up the AddAction
		SessionManager.getManager().setSession(testUtil.getSession());
		AddAction addAction = new AddAction();
		TreePath path = testUtil.getRandomTermPath();
		OBOClass parentClass = (OBOClass) ((Link) path.getLastPathComponent())
				.getChild();
		TreePath[] pathArr = { path };
		addAction.clickInit(SelectionManager.createSelectionFromPaths(null,
				pathArr, null, SessionManager.getManager()
						.getCurrentLinkDatabase(), RootAlgorithm.GREEDY, true),
				null);

		IDProfile profile = new NamedIDProfile();

		String[] rules = { "GO:$sequence(7)$", "GO:$sequence(7,100000,200000)$" };

		((DefaultIDGenerator) IDManager.getManager().getIDAdapter())
				.setProfile(profile);

		Collection<HistoryItem> historyItems = new LinkedList<HistoryItem>();

		Set newTerms = new HashSet();

		Map itemObjectMap = new HashMap();

		int newTermCount = 100;
		for (int i = 0; i < newTermCount; i++) {
			profile.setDefaultRule(rules[i % rules.length]);
			HistoryItem item = addAction.execute();
			historyItems.add(item);
			testUtil.apply(item);
			Set tempSet = new HashSet();
			tempSet.addAll(TermUtil.getTerms(testUtil.getSession()));
			tempSet.removeAll(oldTerms);
			tempSet.removeAll(newTerms);
			assertTrue("Exactly one new term should be created per cycle",
					tempSet.size() == 1);
			OBOClass newTerm = (OBOClass) tempSet.iterator().next();
			newTerms.add(newTerm);
			itemObjectMap.put(item, newTerm);
		}
		/*
		 * Set newTerms = new HashSet();
		 * newTerms.addAll(testUtil.getSession().getTerms());
		 * newTerms.removeAll(oldTerms);
		 */
		assertTrue("Should have created " + newTermCount
				+ " terms, but actually " + "created " + newTerms.size(),
				newTerms.size() == newTermCount);

		it = newTerms.iterator();
		while (it.hasNext()) {
			OBOClass oboClass = (OBOClass) it.next();
			assertNotNull("Null ids should not be generated", oboClass.getID());
			assertTrue("Unexpected id generated!", expectedIDs
					.contains(oboClass.getID()));
			assertTrue("Each new term should have only one parent", oboClass
					.getParents().size() == 1);
			Link parentLink = oboClass.getParents().iterator().next();
			assertTrue("Parent class should be " + parentClass
					+ ", but actually " + "is " + parentLink.getParent(),
					parentClass.equals(parentLink.getParent()));
			assertTrue("Link type should be is_a", parentLink.getType().equals(
					OBOProperty.IS_A));
			assertTrue("Namespace should be " + parentClass.getNamespace()
					+ ", but actually is " + oboClass.getNamespace(),
					ObjectUtil.equals(oboClass.getNamespace(), parentClass
							.getNamespace()));
		}

		it = historyItems.iterator();
		while (it.hasNext()) {
			Collection<OBOClass> terms = TermUtil.getTerms(testUtil
					.getSession());
			int termCount = terms.size();
			HistoryItem item = (HistoryItem) it.next();
			OBOClass currentClass = (OBOClass) itemObjectMap.get(item);
			assertTrue("Current object collection should contain "
					+ currentClass, terms.contains(currentClass));
			testUtil.reverse(item);
			assertTrue("Current object collection should not contain "
					+ currentClass, !terms.contains(currentClass));
		}

		Collection<OBOClass> terms = TermUtil.getTerms(testUtil.getSession());
		assertTrue("Term set should be the same size as before; oldSize = "
				+ oldTerms.size() + ", currentSize = " + terms.size(), oldTerms
				.size() == terms.size());

		assertTrue("Term set should contain all the same elements as before.",
				terms.containsAll(oldTerms));
	}
}
