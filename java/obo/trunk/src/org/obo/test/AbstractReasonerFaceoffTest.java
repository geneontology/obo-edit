package org.obo.test;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
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

	public void testLinks() {
		int i = 0;
		for (IdentifiedObject io : reference.getObjects()) {
			System.out.println("Checking term " + (i++) + " of "
					+ reference.getObjects().size());
			if (io instanceof LinkedObject) {
				for (Link link : reference.getParents((LinkedObject) io)) {
					Collection<Explanation> refExps = reference
							.getExplanations(link);
					for (ReasonedLinkDatabase reasoner : testReasoners) {
						boolean contains = TermUtil
								.containsLink(reasoner, link);
						Collection<Explanation> exps = reasoner.getExplanations(link);
						boolean containsExps = !TermUtil.isImplied(link) || exps.equals(refExps);
						if (!contains) {
							System.err.println("Reasoner " + reasoner
									+ " does not contain reference " + link);
							System.err.println("all parents of "
									+ link.getChild() + ": ");
							for (Link p : reasoner.getParents(link.getChild()))
								System.err.println("   " + p);
						}
						assertTrue("Reasoner " + reasoner
								+ " does not contain reference " + link,
								contains);
						// assertTrue("Reasoner " + reasoner +" has different explanations for "+link, containsExps);
					}
				}
			}
		}
	}
}
