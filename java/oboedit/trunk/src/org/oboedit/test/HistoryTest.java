package org.oboedit.test;

import junit.framework.*;

import java.util.*;

import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.datamodel.impl.SynonymCategoryImpl;
import org.obo.datamodel.impl.TermCategoryImpl;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.AddSynonymHistoryItem;
import org.obo.history.CardinalityHistoryItem;
import org.obo.history.CategoryChangeHistoryItem;
import org.obo.history.ChangeSynCategoryHistoryItem;
import org.obo.history.ChangeSynScopeHistoryItem;
import org.obo.history.CommentChangeHistoryItem;
import org.obo.history.CompletesHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.CyclicHistoryItem;
import org.obo.history.DefaultHistoryList;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.DomainHistoryItem;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.InverseNecHistoryItem;
import org.obo.history.LinkTypeHistoryItem;
import org.obo.history.MaxCardinalityHistoryItem;
import org.obo.history.MinCardinalityHistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.NamespaceHistoryItem;
import org.obo.history.NecessarilyTrueHistoryItem;
import org.obo.history.ObsoleteObjectHistoryItem;
import org.obo.history.OperationModel;
import org.obo.history.RangeHistoryItem;
import org.obo.history.SecondaryIDHistoryItem;
import org.obo.history.SymmetricHistoryItem;
import org.obo.history.SynonymCategoryHistoryItem;
import org.obo.history.TRNamespaceHistoryItem;
import org.obo.history.TermCategoryHistoryItem;
import org.obo.history.TermMergeHistoryItem;
import org.obo.history.TermMoveHistoryItem;
import org.obo.history.TermNamespaceHistoryItem;
import org.obo.history.TermSplitHistoryItem;
import org.obo.history.TransitiveHistoryItem;
import org.obo.util.HistoryUtil;
import org.obo.util.TermUtil;

public class HistoryTest extends TestCase {

	protected OBOSession session;
	protected OBOClass term;
	protected OBOClass randomTerm;
	protected OBOClass go110Term;
	protected OperationModel operationModel;
	protected TestUtil testUtil;

	protected static Link randomLink;

	protected static interface HistoryItemCheck {
		public void check(HistoryItem item, boolean reverseIt);
	}

	public HistoryTest(String s) {
		super(s);
	}

	public static Test suite() {
		TestSuite suite = new TestSuite();
		for (int i = 0; i < 1; i++)
			suite.addTest(new HistoryTest("testHistoryTracker"));

		return new TestSuite(HistoryTest.class);
	}

	public HistoryList createHistoryList() {
		HistoryList out = new DefaultHistoryList();
		OBOClass randomTerm = testUtil.getRandomClass();
		OBOClass randomTerm2 = testUtil.getRandomClass();
		OBOClass randomTerm3 = testUtil.getRandomClass();
		OBOProperty randomProperty = testUtil.getRandomProperty();
		randomLink = testUtil.getRandomLink();
		Link randomLink2 = testUtil.getRandomLink();
		assertNotNull(randomProperty);

		if (randomTerm.equals(randomTerm2) || randomTerm2.equals(randomTerm3)
				|| randomTerm.equals(randomTerm3)) {
			System.err.println("duplicate random terms!!!");
			System.exit(1);
		}

		out.addItem(new SynonymCategoryHistoryItem(null,
				new SynonymCategoryImpl("special", "my special category",
						Synonym.UNKNOWN_SCOPE), true, false));

		out.addItem(new TermCategoryHistoryItem(null, new TermCategoryImpl(
				"mysubset", "my subset"), true, false));

		out.addItem(new CategoryChangeHistoryItem("mysubset", false, randomTerm
				.getID()));

		out.addItem(new AddSynonymHistoryItem(randomTerm.getID(),
				"Extra special synonym"));

		out.addItem(new ChangeSynCategoryHistoryItem(randomTerm.getID(),
				"Extra special synonym", null, "special"));
		out.addItem(new ChangeSynScopeHistoryItem(randomTerm.getID(),
				"Extra special synonym", Synonym.RELATED_SYNONYM,
				Synonym.BROAD_SYNONYM));

		out.addItem(new AddDbxrefHistoryItem(randomTerm.getID(),
				new DbxrefImpl("monkey", "mash"), false,
				"Extra special synonym"));
		out.addItem(new CommentChangeHistoryItem(null, "Get a load of this!",
				randomTerm.getID()));
		out.addItem(new DefinitionChangeHistoryItem(randomTerm.getDefinition(),
				"Redfined as AWESOME!", randomTerm.getID()));
		out.addItem(new NameChangeHistoryItem(randomTerm, "Moooonsooongyana!"));
		out.addItem(new TermNamespaceHistoryItem(null, "elmo", true, false));
		out
				.addItem(new NamespaceHistoryItem(randomTerm, new Namespace(
						"elmo")));
		out.addItem(new SecondaryIDHistoryItem(randomTerm, "stupid:otherthing",
				false));

		out.addItem(new CyclicHistoryItem(randomProperty));
		out.addItem(new SymmetricHistoryItem(randomProperty));
		out.addItem(new TransitiveHistoryItem(testUtil.getRandomProperty()));

		out.addItem(new CreateObjectHistoryItem("new:term", OBOClass.OBO_CLASS
				.getID()));
		out.addItem(new DomainHistoryItem(randomProperty, randomTerm));
		out.addItem(new RangeHistoryItem(randomProperty, randomTerm));

		out.addItem(new CardinalityHistoryItem((OBORestriction) randomLink,
				new Integer(3)));

		out.addItem(new MaxCardinalityHistoryItem((OBORestriction) randomLink2,
				new Integer(15)));

		out.addItem(new MinCardinalityHistoryItem((OBORestriction) randomLink2,
				new Integer(12)));

		out.addItem(new InverseNecHistoryItem((OBORestriction) randomLink));

		out
				.addItem(new NecessarilyTrueHistoryItem(
						(OBORestriction) randomLink));

		out.addItem(new CompletesHistoryItem((OBORestriction) randomLink));

		out.addItem(new LinkTypeHistoryItem(randomLink,
				OBOProperty.DISJOINT_FROM));

		out.addItem(new ObsoleteObjectHistoryItem("new:term"));

		out.addItem(new TRNamespaceHistoryItem(testUtil.getRandomLink(),
				new Namespace("elmo")));

		out.addItem(new CreateLinkHistoryItem(randomTerm3, OBOProperty.IS_A,
				randomTerm));

		out.addItem(new TermMoveHistoryItem(testUtil.getRandomClass(), testUtil
				.getRandomLink()));

		out.addItem(new TermMergeHistoryItem(testUtil.getRandomClass(),
				testUtil.getRandomClass()));

		out.addItem(new TermSplitHistoryItem(testUtil.getRandomClass(),
				"out:split", false));

		return out;
	}

	protected void doHistoryTest(HistoryItem item, HistoryItemCheck check) {
		doHistoryTest(item, null, check);
	}

	protected void doHistoryTest(HistoryItem forwardItem,
			HistoryItem reverseItem, HistoryItemCheck check) {
		operationModel.apply(forwardItem);
		check.check(forwardItem, false);

		if (reverseItem != null) {
			operationModel.apply(reverseItem);
			check.check(reverseItem, true);

			operationModel.reverse(reverseItem);
			check.check(reverseItem, false);
		}

		operationModel.reverse(forwardItem);
		check.check(forwardItem, true);

	}

	@Override
	public void setUp() throws Exception {
		OBOFileAdapter adapter = new OBOFileAdapter();
		// URL testFile = ClassLoader.getSystemClassLoader().getResource(
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add("test_resources/testfile.1.0.obo");
		session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);
		testUtil = new TestUtil(session);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);

		randomTerm = testUtil.getRandomClass();
		go110Term = (OBOClass) session.getObject("GO:0000110");
	}

	@Override
	public void tearDown() throws Exception {
		session = null;
		randomTerm = null;
		operationModel = null;
		go110Term = null;
	}

	protected void checkForSelfLinks(OBOSession session) throws Exception {
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				Iterator it2 = lo.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					assertTrue("Self link " + link + " is not allowed!", !link
							.getParent().equals(link.getChild()));
				}
			}
		}
	}

	public void testHistoryTracker() throws Exception {
		Iterator it;
		// get the history list and apply it to an ontology
		HistoryList historyList = createHistoryList();
		OBOSession changed = TestUtil.createSession();
		assertNotNull("Test sessions shouldn't be null", changed);
		TestUtil testUtilC = new TestUtil(changed);
		testUtilC.apply(historyList);
		checkForSelfLinks(changed);

		assertTrue("Plain applied ontology should not contain null categories",
				!changed.getCategories().contains(null));

		// get a copy of the original ontology, and use the history
		// generate to generate its own version of the changes
		session = TestUtil.createSession();
		TestUtil testUtilS = new TestUtil(session);
		HistoryList generatedList = HistoryGenerator.getHistory(session,
				changed);

		NecessarilyTrueHistoryItem nec_item = null;
		it = generatedList.getHistoryItems();
		while (it.hasNext()) {
			HistoryItem item = (HistoryItem) it.next();
			if (item instanceof NecessarilyTrueHistoryItem) {
				nec_item = (NecessarilyTrueHistoryItem) item;
				break;
			}
		}
		assertTrue("There should be at least one non-necessary history item.",
				nec_item != null);

		// apply the history generator changes to the new ontology
		testUtilS.apply(generatedList);
		checkForSelfLinks(session);

		Link realRel = HistoryTest.findRel(HistoryUtil.getRealRel(session, nec_item
				.getRel()));
		Link oldRel = HistoryUtil.getRealRel(changed, nec_item.getRel());
		LinkedObject delChild = (LinkedObject) session.getObject(randomLink
				.getChild().getID());

		assertTrue("randomLink " + randomLink
				+ " should not exist in session ontology", HistoryUtil
				.findParentRel(randomLink, delChild) == null);
		assertTrue("necessarily true status of " + nec_item.getRel()
				+ " should be false because of " + nec_item + ", realRel = "
				+ realRel + ", isNec="
				+ (((OBORestriction) realRel).isNecessarilyTrue()), !HistoryTest
				.isNecessary(realRel));
		assertTrue("is complete status of " + nec_item.getRel()
				+ " should be true", TermUtil.isIntersection(oldRel));

		// here's the problem; the

		assertTrue("Generated changes ontology should not contain null "
				+ "categories", !session.getCategories().contains(null));

		// see if the changes match
		HistoryList troubleList = HistoryGenerator.getHistory(session, changed);
		assertTrue("The history generator should find no differences: "
				+ "troubleList = " + troubleList, troubleList
				.size() == 0);
		assertTrue("The sessions should contain the same number of "
				+ "objects after histories are applied", session.getObjects()
				.size() == changed.getObjects().size());
		it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			IdentifiedObject io2 = changed.getObject(io
					.getID());
			assertNotNull("Every object in one ontology should have a "
					+ "corresponding object in the other. " + io.getID(), io2);
			assertTrue("Corresponding objects must implement the same "
					+ "interfaces. " + io.getID(),
					io instanceof LinkedObject == io2 instanceof LinkedObject);
			assertTrue("Corresponding objects must have the same names: "
					+ io.getID(), io.getName().equals(io2.getName()));
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				LinkedObject lo2 = (LinkedObject) io2;
				assertTrue("Corresponding objects should have the same "
						+ "number of parents: " + lo.getID(), lo.getParents()
						.size() == lo2.getParents().size());
			}
		}
	}
	/*
	 * public void testHistorySave() throws Exception { HistoryList historyList =
	 * createHistoryList(); XMLHistoryAdapter adapter = new XMLHistoryAdapter();
	 * FileAdapterConfiguration config = new FileAdapterConfiguration();
	 * config.setWritePath("/home/jrichter/history.xml");
	 * adapter.doOperation(OBOEditAdapter.WRITE_HISTORY, config, historyList); }
	 * 
	 * public void testHistoryLoad() throws Exception { XMLHistoryAdapter
	 * adapter = new XMLHistoryAdapter(); FileAdapterConfiguration config = new
	 * FileAdapterConfiguration("/home/jrichter/history.xml"); List histories =
	 * (List) adapter. doOperation(OBOEditAdapter.READ_HISTORY, config, null);
	 * Iterator it2 = histories.iterator(); while(it2.hasNext()) { HistoryList
	 * historyList = (HistoryList) it2.next(); System.err.println("Applying
	 * history "+historyList); Iterator it = historyList.getHistoryItems();
	 * while(it.hasNext()) { HistoryItem item = (HistoryItem) it.next();
	 * operationModel.apply(item); } } }
	 */

	public static boolean isNecessary(Link link) {
		if (link instanceof OBORestriction)
			return ((OBORestriction) link).isNecessarilyTrue();
		else
			return false;
	
	}

	public static Link findRel(Link tr) {
		return HistoryUtil.findParentRel(tr, tr.getChild());
	}
}
