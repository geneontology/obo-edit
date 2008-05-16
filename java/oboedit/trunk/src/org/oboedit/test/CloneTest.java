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

public class CloneTest extends TestCase {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CloneTest.class);

	public void testClone() {
		Iterator it;
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

		Set newTerms = new HashSet();

		Map itemObjectMap = new HashMap();

		int newTermCount = 10;  // ?
		for (int i = 0; i < newTermCount; i++) {
			HistoryItem item = cloneAction.execute();
			historyItems.add(item);
			testUtil.apply(item);
			Set tempSet = new HashSet();
			tempSet.addAll(TermUtil.getTerms(testUtil.getSession()));
			tempSet.removeAll(oldTerms);
			tempSet.removeAll(newTerms);
			assertTrue("Exactly one new term should be created per cycle",
				   tempSet.size() == 1);  // ?
			OBOClass newTerm = (OBOClass) tempSet.iterator().next();
			newTerms.add(newTerm);
			itemObjectMap.put(item, newTerm);
		}

		assertTrue("Should have created " + newTermCount
			   + " terms; actually created " + newTerms.size(),
			   newTerms.size() == newTermCount);

		it = newTerms.iterator();
		while (it.hasNext()) {
			OBOClass oboClass = (OBOClass) it.next();
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

		it = historyItems.iterator();
		while (it.hasNext()) {
			Collection<OBOClass> terms = TermUtil.getTerms(testUtil
					.getSession());
			int termCount = terms.size();
			HistoryItem item = (HistoryItem) it.next();
			OBOClass currentClass = (OBOClass) itemObjectMap.get(item);
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
