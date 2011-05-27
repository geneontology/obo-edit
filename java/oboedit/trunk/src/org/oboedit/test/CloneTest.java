package org.oboedit.test;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import javax.swing.tree.TreePath;

import junit.framework.TestCase;

import org.apache.log4j.Logger;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.RootAlgorithm;
import org.obo.history.HistoryItem;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.actions.CloneAction;

public class CloneTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CloneTest.class);

	public void testClone() {
		TestUtil testUtil = TestUtil.getInstance();

		// copy the original set of terms
		Set<LinkedObject> oldTerms = new HashSet<LinkedObject>();
		oldTerms.addAll(TermUtil.getTerms(testUtil.getSession()));

		// set up the CloneAction
		SessionManager.getManager().setSession(testUtil.getSession());
		CloneAction cloneAction = new CloneAction();
		TreePath path = testUtil.getRandomTermPath();
		TreePath[] pathArr = { path };
		OBOClass parentClass = (OBOClass) ((Link) path.getLastPathComponent()).getChild();
		cloneAction.clickInit(SelectionManager.
				      createSelectionFromPaths(null,
							       pathArr, null, SessionManager.getManager()
							       .getCurrentLinkDatabase(), RootAlgorithm.GREEDY, true),
				      null);

		Collection<HistoryItem> historyItems = new LinkedList<HistoryItem>();

		Set<OBOClass> newTerms = new HashSet<OBOClass>();

		Map<HistoryItem, OBOClass> itemObjectMap = new HashMap<HistoryItem, OBOClass>();

		int newTermCount = 10;  // ?
		for (int i = 0; i < newTermCount; i++) {
			HistoryItem item = cloneAction.execute();
			historyItems.add(item);
			testUtil.apply(item);
			Set<OBOClass> tempSet = new HashSet<OBOClass>();
			tempSet.addAll(TermUtil.getTerms(testUtil.getSession()));
			tempSet.removeAll(oldTerms);
			tempSet.removeAll(newTerms);
			assertTrue("Exactly one new term should be created per cycle",
				   tempSet.size() == 1);  // ?
			OBOClass newTerm = tempSet.iterator().next();
			newTerms.add(newTerm);
			itemObjectMap.put(item, newTerm);
		}

		assertTrue("Should have created " + newTermCount
			   + " terms; actually created " + newTerms.size(),
			   newTerms.size() == newTermCount);

		for(OBOClass oboClass : newTerms) {
			assertNotNull("Null ids should not be generated", oboClass.getID());
			// Terms don't necessarily have just one parent.  Need better test.
//			assertTrue("Each new term should have only one parent--" + oboClass.getName() + " has these parents:\n " +
//				   oboClass.getParents(),
//				   oboClass.getParents().size() == 1);
			Link parentLink = oboClass.getParents().iterator().next();
			// !! Need to compare original term's parent with clone's parent
//			OBOClass parentClass = (OBOClass) ((Link) path.getLastPathComponent()).getChild();
//			assertTrue("Parent class of " + oboClass + " should be " + parentClass
//				   + ", but actually " + "is " + parentLink.getParent(),
//				   parentClass.equals(parentLink.getParent()));
			assertTrue("Link type should be is_a", parentLink.getType().equals(
					   OBOProperty.IS_A));
			assertTrue("Namespace should be " + parentClass.getNamespace()
				   + ", but actually is " + oboClass.getNamespace(),
				   ObjectUtil.equals(oboClass.getNamespace(), parentClass
						     .getNamespace()));
		}

		for(HistoryItem item : historyItems) {
			Collection<OBOClass> terms = TermUtil.getTerms(testUtil
					.getSession());
			int termCount = terms.size();
			OBOClass currentClass = itemObjectMap.get(item);
			logger.info("currentClass = " + item); // DEL
			assertTrue("Current object collection should contain " + currentClass,
				   terms.contains(currentClass));
			testUtil.reverse(item);
			assertTrue("Current object collection should not contain a class with name = " +
				   currentClass.getName() + ", id = " + currentClass.getID(),
				   !terms.contains(currentClass));
		}

		Collection<OBOClass> terms = TermUtil.getTerms(testUtil.getSession());
		assertTrue("Term set should be the same size as before; oldSize = "
				+ oldTerms.size() + ", currentSize = " + terms.size(), oldTerms
				.size() == terms.size());

		assertTrue("Term set should contain all the same elements as before.",
				terms.containsAll(oldTerms));
	}
}
