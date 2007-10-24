package org.oboedit.test;

import junit.framework.*;

import java.util.*;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.history.DefaultHistoryList;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;

public abstract class OperationTest extends TestCase {

	protected TestUtil testUtil;
	protected OBOSession session;

	public void setSession(OBOSession session) {
		this.session = session;
		testUtil = new TestUtil(session);
	}

	public static class TestBundle {
		protected HistoryItem item;
		protected Collection objectPairs;

		public TestBundle(HistoryItem item, Collection objectPairs) {
			this.item = item;
			this.objectPairs = objectPairs;
		}

		public HistoryItem getHistoryItem() {
			return item;
		}

		public Collection getObjectPairs() {
			return objectPairs;
		}
	}

	public static class ObjectPair implements Cloneable {
		protected IdentifiedObject original;
		protected IdentifiedObject result;

		public ObjectPair(IdentifiedObject original, IdentifiedObject result) {
			this.original = original;
			this.result = result;
		}

		public IdentifiedObject getOriginal() {
			return original;
		}

		public IdentifiedObject getResult() {
			return result;
		}

		@Override
		public Object clone() {
			try {
				ObjectPair clone = (ObjectPair) super.clone();
				clone.original = (IdentifiedObject) original.clone();
				clone.result = (IdentifiedObject) result.clone();
				return clone;
			} catch (CloneNotSupportedException ex) {
				return null;
			}
		}
	}

	protected abstract Collection getTestBundles();

	@Override
	public void setUp() throws Exception {
		setSession(TestUtil.createSession());
	}

	public void testOperation() throws Exception {
		Iterator it = getTestBundles().iterator();
		while (it.hasNext()) {
			TestBundle testBundle = (TestBundle) it.next();
			doBundleTest(testBundle);
		}
	}

	public void doBundleTest(TestBundle testBundle) throws Exception {
		Iterator it;

		// get the history generator version of the changes
		HistoryList allChanges = new DefaultHistoryList();
		it = testBundle.getObjectPairs().iterator();
		while (it.hasNext()) {
			ObjectPair op = (ObjectPair) it.next();
			if (op.getOriginal() != null && op.getResult() != null) {
				HistoryGenerator.getChanges(op.getOriginal(), op.getResult(),
						allChanges);
			} else if (op.getOriginal() == null) {
				// add some code to deal with new term creation
			} else if (op.getResult() == null) {
				// add some code to deal with term destruction
			}
		}
		// make sure there's at least some difference in the original
		// and result objects
		assertTrue("At least some changes should be made in a test bundle",
				allChanges.size() > 0);

		// apply the relevant history item
		testUtil.apply(testBundle.getHistoryItem());

		// make sure the expected outcomes match the real outcomes
		it = testBundle.getObjectPairs().iterator();
		while (it.hasNext()) {
			ObjectPair op = (ObjectPair) it.next();
			// NOTICE: We need special handling for term generation

			IdentifiedObject modifiedObject = session
					.getObject(op.getOriginal().getID());
			if (modifiedObject != null && op.getResult() != null) {
				HistoryList changes = HistoryGenerator.getChanges(op
						.getResult(), modifiedObject);
				assertTrue("Found differences between expected outcome and "
						+ "actual outcome " + changes, changes
						.size() == 0);
			}
		}

		// undo the changes
		testUtil.reverse(testBundle.getHistoryItem());

		// make sure the undone values match the original values
		it = testBundle.getObjectPairs().iterator();
		while (it.hasNext()) {
			ObjectPair op = (ObjectPair) it.next();
			IdentifiedObject modifiedObject = session
					.getObject(op.getOriginal().getID());
			if (modifiedObject != null && op.getResult() != null) {
				HistoryList changes = HistoryGenerator.getChanges(op
						.getOriginal(), modifiedObject);
				assertTrue("Found differences between original object "
						+ "and object after undo: " + changes,
						changes.size() == 0);
			}
		}

		// apply the history generator version of the changes
		it = allChanges.getHistoryItems();
		while (it.hasNext()) {
			HistoryItem item = (HistoryItem) it.next();
			testUtil.apply(item);
		}

		// make sure the expected outcomes match the real outcomes
		it = testBundle.getObjectPairs().iterator();
		while (it.hasNext()) {
			ObjectPair op = (ObjectPair) it.next();
			// NOTICE: We need special handling for term generation

			IdentifiedObject modifiedObject = session
					.getObject(op.getOriginal().getID());
			if (modifiedObject != null && op.getResult() != null) {
				HistoryList changes = HistoryGenerator.getChanges(op
						.getResult(), modifiedObject);
				assertTrue("Found differences between expected outcome and "
						+ "actual outcome when applying history generator"
						+ " changes: " + changes, changes
						.size() == 0);
			}
		}

		// reverse history generator version of the changes
		List allChangesList = new ArrayList();
		Iterator it2 = allChanges.getHistoryItems();
		while(it2.hasNext()) {
			allChangesList.add(it2.next());
		}
		Collections.reverse(allChangesList);
		it = allChangesList.iterator();
		while (it.hasNext()) {
			HistoryItem item = (HistoryItem) it.next();
			testUtil.reverse(item);
		}

		// make sure the undone values match the original values
		it = testBundle.getObjectPairs().iterator();
		while (it.hasNext()) {
			ObjectPair op = (ObjectPair) it.next();
			IdentifiedObject modifiedObject = session
					.getObject(op.getOriginal().getID());
			if (modifiedObject != null && op.getResult() != null) {
				HistoryList changes = HistoryGenerator.getChanges(op
						.getOriginal(), modifiedObject);
				assertTrue("Found differences between original object "
						+ "and object after undoing history generator "
						+ "list items: " + changes, changes
						.size() == 0);
			}
		}
	}
}
