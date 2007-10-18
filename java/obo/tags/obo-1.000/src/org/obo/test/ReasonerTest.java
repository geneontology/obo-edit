package org.obo.test;

import junit.framework.*;

import java.io.PrintStream;
import java.util.*;

import org.bbop.io.AuditedPrintStream;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.CompletesHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.StringRelationship;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.impl.ReasonerOperationModel;

public class ReasonerTest extends TestCase {

	protected OBOSession session;

	public ReasonerTest(String name) {
		super(name);
	}
	public static final String SO_XP_PATH="lib/resources/so-xp.obo";
	
	public static Test suite() {
		PrintStream audited = new AuditedPrintStream(System.err, 25, true);

		System.setErr(audited);
		TestSuite suite = new TestSuite();
		suite.addTest(new ReasonerTest("testBrokenAddLink"));
		suite.addTest(new ReasonerTest("testBrokenAddLink2"));
		suite.addTest(new ReasonerTest("testBrokenCompleteChange"));
		for (int i = 0; i < 200; i++) {
			suite.addTest(new ReasonerTest("testAddNewLink"));
			suite.addTest(new ReasonerTest("testCompleteLinkChange"));
			suite.addTest(new ReasonerTest("testLinkDel"));
			suite.addTest(new ReasonerTest("testCompleteLinkDel"));
		}

		return suite;
	}

	@Override
	public void setUp() throws Exception {
		ForwardChainingReasoner.checkRecache = false;
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(SO_XP_PATH);
		session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config,
				null);

		// SessionManager.getManager().setSession(session);
	}

	@Override
	public void tearDown() throws Exception {
		session = null;
	}

	public void testCompleteLinkDel() throws Exception {
		Collection items = new LinkedList();
		for (int i = 0; i < 1; i++) {
			Link link = getRandomCompleteLink();
			if (link == null) {
				System.err
						.println("can't do complete link test; no complete links in ontology");
				return;
			}
			items.add(new DeleteLinkHistoryItem(link));
		}
		doLinkTest(items);
	}

	public void testLinkDel() throws Exception {
		Collection items = new LinkedList();
		for (int i = 0; i < 5; i++) {
			Link link = getRandomLink();
			items.add(new DeleteLinkHistoryItem(link));
		}
		doLinkTest(items);
	}

	public void testBrokenCompleteChange() throws Exception {
		StringRelationship sr = new StringRelationship(
				"SO:0000913", "has_quality", "SO:0000756");
		// HistoryItem.StringRelationship sr = new
		// HistoryItem.StringRelationship("SO:0000497", "SO:0000556",
		// "part_of");
		HistoryItem item = new CompletesHistoryItem(sr, false);
		Collection items = new LinkedList();
		items.add(item);
		doLinkTest(items);
	}

	public void testBrokenLinkDel() throws Exception {
		Collection items = new LinkedList();
		items.add(new DeleteLinkHistoryItem(new StringRelationship(
				"SO:0000665", "has_quality", "SO:0000878", true)));
		doLinkTest(items);
	}

	public void testBrokenAddLink() throws Exception {
		Collection items = new LinkedList();
		/*
		 * Copied SO:0000895 to SO:0000507 with type OBO_REL:is_a Copied
		 * SO:0000331 to SO:0000360 with type OBO_REL:is_a Copied SO:0000816 to
		 * SO:0000515 with type OBO_REL:is_a Copied SO:2000061 to SO:0000466
		 * with type OBO_REL:is_a Copied SO:0000877 to SO:1000101 with type
		 * OBO_REL:is_a
		 */

		items.add(new CreateLinkHistoryItem("SO:0000895", "OBO_REL:is_a",
				"SO:0000507"));
		items.add(new CreateLinkHistoryItem("SO:0000331", "OBO_REL:is_a",
				"SO:0000360"));
		items.add(new CreateLinkHistoryItem("SO:0000816", "OBO_REL:is_a",
				"SO:0000515"));
		items.add(new CreateLinkHistoryItem("SO:2000061", "OBO_REL:is_a",
				"SO:0000466"));
		items.add(new CreateLinkHistoryItem("SO:0000877", "OBO_REL:is_a",
				"SO:1000101"));
		doLinkTest(items);
	}

	public void testBrokenAddLink2() throws Exception {
		Collection items = new LinkedList();
		/*
		 * Copied SO:0000127 to SO:0000201 with type OBO_REL:is_a Copied
		 * SO:0000839 to SO:0000196 with type part_of Copied SO:1000035 to
		 * SO:1000104 with type OBO_REL:is_a Copied SO:0000724 to SO:1000014
		 * with type OBO_REL:is_a Copied SO:0000003 to SO:0000645 with type
		 * OBO_REL:is_a
		 * 
		 * Copied SO:0000884 to SO:0000165 with type OBO_REL:is_a Copied
		 * SO:0000377 to SO:0000844 with type OBO_REL:is_a Copied SO:1000043 to
		 * SO:1000075 with type OBO_REL:is_a Copied SO:0000930 to SO:0000234
		 * with type part_of Copied SO:0000548 to SO:0000470 with type
		 * associated_with
		 * 
		 * Copied SO:0000108 to SO:0000199 with type has_quality Copied
		 * SO:0000985 to SO:0000928 with type OBO_REL:is_a Copied SO:0000470 to
		 * SO:0000832 with type part_of Copied SO:0000060 to SO:0000466 with
		 * type OBO_REL:is_a Copied SO:0000704 to SO:1000048 with type member_of
		 */

		items.add(new CreateLinkHistoryItem("SO:1000075", "OBO_REL:is_a",
				"SO:0000140"));
		items.add(new CreateLinkHistoryItem("SO:0000690", "OBO_REL:is_a",
				"SO:1000083"));
		items.add(new CreateLinkHistoryItem("SO:0000677", "OBO_REL:is_a",
				"SO:0000574"));
		items.add(new CreateLinkHistoryItem("SO:1000052", "OBO_REL:is_a",
				"SO:0000470"));
		items.add(new CreateLinkHistoryItem("SO:0000235", "OBO_REL:is_a",
				"SO:1000036"));
		doLinkTest(items);
	}

	public void testCompleteLinkChange() throws Exception {
		Link link = getRandomLink();
		System.err.println("doing check on " + link);
		doLinkTest(link, new CompletesHistoryItem((OBORestriction) link));
	}

	public void testAddNewLink() throws Exception {
		Collection<HistoryItem> items = new LinkedList<HistoryItem>();
		for (int i = 0; i < 5; i++) {
			Link link = getRandomLink();
			Link link2 = getRandomLink();
			System.err.println("creating link " + link2.getChild() + " -"
					+ link.getType() + "-> " + link.getParent());
			items.add(new CreateLinkHistoryItem(link2.getChild(), link.getType(),
					link.getParent()));
		}
		doLinkTest(items);
	}

	public void doLinkTest(Link link) throws Exception {
		doLinkTest(link, new DeleteLinkHistoryItem(link));
	}

	public void doLinkTest(Link link, HistoryItem item) throws Exception {
		doLinkTest(item);
	}

	public void doLinkTest(HistoryItem item) throws Exception {
		doLinkTest(Collections.singleton(item));
	}

	public void doLinkTest(Collection items) throws Exception {
		ForwardChainingReasoner.weirdLink = null;
		ForwardChainingReasoner.checkRecache = false;
		Iterator it;

		DefaultLinkDatabase linkDatabase = new DefaultLinkDatabase(session);
		ReasonedLinkDatabase reasonedDB = new ForwardChainingReasoner();
		reasonedDB.setLinkDatabase(linkDatabase);

		ReasonedLinkDatabase reasoner = reasonedDB;

		ReasonerOperationModel opModel = new ReasonerOperationModel(reasoner);
		opModel.setSession(session);

		reasoner.recache();

		DefaultOperationModel model = new DefaultOperationModel();
		model.setSession(session);
		long incrementalTime = 0;
		long time;
		it = items.iterator();
		for (int i = 0; it.hasNext(); i++) {
			HistoryItem item = (HistoryItem) it.next();
			System.err.println("applying " + item);
			model.apply(item);
			time = System.currentTimeMillis();
			opModel.apply(item);
			incrementalTime += System.currentTimeMillis() - time;
		}

		Map objCache = new LinkedHashMap();

		it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				Collection dynamicParents = new LinkedHashSet();
				dynamicParents.addAll(reasoner.getParents(lo));
				objCache.put(lo, dynamicParents);
			}
		}
		time = System.currentTimeMillis();
		reasoner.recache();
		System.err.println("recache time    : "
				+ (System.currentTimeMillis() - time) + " ms");
		System.err.println("incremental time: " + incrementalTime + " ms");

		String failureMessage = "";
		it = session.getObjects().iterator();
		for (int i = 0; it.hasNext(); i++) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				Collection dynamicParents = (Set) objCache.get(lo);
				Collection dynamicParentsCopy = new LinkedList(dynamicParents);
				Collection<Link> cacheParents = new LinkedHashSet<Link>();
				Collection fullReasonerParents = reasoner.getParents(lo);
				cacheParents.addAll(fullReasonerParents);
				cacheParents.removeAll(dynamicParents);

				if (cacheParents.size() > 0) {
					if (failureMessage.length() > 0)
						failureMessage += "\n";
					else
						failureMessage = "On application of history items "
								+ items + "...\n";
					String parentsMessage = "";
					Iterator it2 = cacheParents.iterator();
					boolean first = true;
					Link parent = null;
					while (it2.hasNext()) {
						parent = (Link) it2.next();
						if (first) {
							first = false;
						} else
							parentsMessage += ", ";
						parentsMessage += parent.toString();
						Iterator it3 = reasoner.getExplanations(parent)
								.iterator();
						while (it3.hasNext()) {
							Explanation exp = (Explanation) it3.next();
							parentsMessage += "\n         exp = " + exp;
						}
					}

					OBOFileAdapter adapter = new OBOFileAdapter();
					OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
					config.getReadPaths().add(SO_XP_PATH);
					OBOSession tempSession = (OBOSession) adapter.doOperation(
							OBOAdapter.READ_ONTOLOGY, config, null);
					DefaultOperationModel tempModel = new DefaultOperationModel();
					tempModel.setSession(tempSession);
					it = items.iterator();
					while (it.hasNext()) {
						HistoryItem item = (HistoryItem) it.next();
						System.err.println("reapplying " + item);
						tempModel.apply(item);
					}
					DefaultLinkDatabase tempLinkDatabase = new DefaultLinkDatabase(
							tempSession);
					ForwardChainingReasoner tempReasonedDB = new ForwardChainingReasoner();
					tempReasonedDB.setLinkDatabase(tempLinkDatabase);
					tempReasonedDB.checkRecache = true;
					ForwardChainingReasoner.weirdLink = cacheParents.iterator()
							.next();
					tempReasonedDB.recache();

					Collection kids = tempReasonedDB.getChildren(parent
							.getParent());

					it2 = dynamicParents.iterator();
					for (int j = 0; it2.hasNext(); j++) {
						parent = (Link) it2.next();
						parentsMessage += "\n         parent " + (j + 1)
								+ ") = " + parent;
					}
					failureMessage += "    dynamic run unable to generate "
							+ parentsMessage;

				}

			}
		}

		if (failureMessage.length() > 0)
			fail(failureMessage);
	}

	protected Link getRandomCompleteLink() {
		Set completeLinks = new LinkedHashSet();
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				Iterator it2 = lo.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (link instanceof OBORestriction) {
						OBORestriction or = (OBORestriction) link;
						if (or.completes())
							completeLinks.add(or);
					}
				}
			}
		}
		int index = (int) (Math.random() * completeLinks.size());
		if (index >= completeLinks.size())
			return null;
		it = completeLinks.iterator();
		Link cl = null;
		for (int i = 0; i < index + 1; i++) {
			cl = (Link) it.next();
		}
		assertNotNull("null random complete link, index = " + index, cl);
		return cl;
	}

	protected Link getRandomLink() {
		int count = 0;
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				Iterator it2 = lo.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (link instanceof OBORestriction) {
						OBORestriction or = (OBORestriction) link;
						if (!or.completes())
							count++;
					} else
						count++;
				}
			}
		}
		int index = (int) (Math.random() * count);
		it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof LinkedObject) {
				Iterator it2 = ((LinkedObject) io).getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (index == 0)
						return link;
					if (link instanceof OBORestriction) {
						if (!((OBORestriction) link).completes())
							index--;
					} else
						index--;
				}
			}
		}
		return null;
	}
}
