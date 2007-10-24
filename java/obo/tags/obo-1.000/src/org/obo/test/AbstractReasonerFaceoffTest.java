package org.obo.test;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

public abstract class AbstractReasonerFaceoffTest extends AbstractOBOTest {

	protected AbstractReasonerFaceoffTest(String name) {
		super(name);
	}

	Collection<ReasonedLinkDatabase> testReasoners = new LinkedList<ReasonedLinkDatabase>();
	ReasonedLinkDatabase reference;

	public void setReferenceReasoner(ReasonedLinkDatabase reasoner) {
		this.reference = reasoner;
	}

	public void addReasoner(ReasonedLinkDatabase reasoner) {
		testReasoners.add(reasoner);
	}

	protected void runReasoner(ReasonedLinkDatabase reasoner) {
		reasoner.setLinkDatabase(linkDatabase);
		reasoner.recache();
	}

	protected abstract void installTestReasoners();

	@Override
	public void setUp() throws Exception {
		super.setUp();
		installTestReasoners();
		long time;
		for (ReasonedLinkDatabase reasoner : testReasoners) {
			time = System.nanoTime();
			System.out.print("Running test reasoner " + reasoner + " ...");
			System.out.flush();
			runReasoner(reasoner);
			System.out.println("done (" + (System.nanoTime() - time) / 1000000d
					+ " ms)");
		}
		time = System.nanoTime();
		System.out.print("Running reference reasoner...");
		System.out.flush();
		runReasoner(reference);
		System.out.println("done (" + (System.nanoTime() - time) / 1000000d
				+ " ms, explainTime = "
				+ ((ForwardChainingReasoner) reference).explainTime + ")");
		System.out.println("setup complete.");
	}

	protected static void hasAllLinks(ReasonedLinkDatabase checkme,
			ReasonedLinkDatabase againstme, Collection<Link> alreadyChecked,
			boolean checkExplanations) {
		int i = 0;
		for (IdentifiedObject io : checkme.getObjects()) {
			System.out.println("Checking term " + (i++) + " of "
					+ checkme.getObjects().size());
			if (io instanceof LinkedObject) {
				for (Link link : checkme.getParents((LinkedObject) io)) {
					if (alreadyChecked.contains(link))
						continue;
					else
						alreadyChecked.add(link);
					Collection<Explanation> refExps = checkme
							.getExplanations(link);
					boolean contains = TermUtil.containsLink(againstme, link);
					Collection<Explanation> exps = againstme
							.getExplanations(link);
					boolean containsExps = !TermUtil.isImplied(link)
							|| exps.equals(refExps);
					if (!contains) {
						System.err.println("Reasoner " + againstme
								+ " does not contain reference " + link);
						System.err.println("all parents of " + link.getChild()
								+ ": ");
						for (Link p : againstme.getParents(link.getChild()))
							System.err.println("   " + p);
					}
					assertTrue("Reasoner " + againstme
							+ " does not contain reference " + link, contains);
					if (checkExplanations) {
						assertTrue("Reasoner " + againstme
								+ " has different explanations for " + link,
								containsExps);
					}

				}
			}
		}
	}

	public void testLinks() {
		boolean testExplanations = false;
		for (ReasonedLinkDatabase reasoner : testReasoners) {
			Collection<Link> checked = new HashSet<Link>();
			hasAllLinks(reference, reasoner, checked, testExplanations);
			hasAllLinks(reasoner, reference, checked, testExplanations);
		}
	}
}
