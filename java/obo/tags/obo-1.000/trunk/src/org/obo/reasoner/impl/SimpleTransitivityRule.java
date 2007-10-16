package org.obo.reasoner.impl;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

public class SimpleTransitivityRule extends AbstractReasonerRule {

	protected static OBORestriction temp = new OBORestrictionImpl();
	long implicationTime;
	long fetchTime;
	long expConstructTime;

	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		implicationTime = 0;
		fetchTime = 0;
		expConstructTime = 0;
	}

	public Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link link) {
		Collection<Explanation> out = new LinkedList<Explanation>();
		long time;
		time = System.nanoTime();
		Collection<Link> parents = reasoner.getParents(link.getParent());
		fetchTime += System.nanoTime() - time;
		for (Link gpLink : parents) {
			time = System.nanoTime();
			boolean success = ReasonerUtil.generateTransitiveImplication(
					reasoner, temp, link, gpLink);
			implicationTime += System.nanoTime() - time;
			if (success) {
				time = System.nanoTime();
				TransitivityExplanation exp = createExplanation(reasoner, temp,
						link, gpLink);
				expConstructTime += System.nanoTime() - time;
				out.add(exp);
			}
		}
		time = System.nanoTime();
		Collection<Link> children = reasoner.getChildren(link.getChild());
		fetchTime += System.nanoTime() - time;
		for (Link gcLink : children) {
			if (TermUtil.isIntersection(gcLink))
				continue;
			time = System.nanoTime();
			boolean success = ReasonerUtil.generateTransitiveImplication(
					reasoner, temp, gcLink, link);
			implicationTime += System.nanoTime() - time;
			if (success) {
				time = System.nanoTime();
				TransitivityExplanation exp = createExplanation(reasoner, temp,
						gcLink, link);
				expConstructTime += System.nanoTime() - time;
				out.add(exp);
			}
		}
//		time = System.nanoTime();
//		pushLinkDown(reasoner, link, out, new HashSet<Link>());
//		implicationTime += System.nanoTime() - time;
		return out;
	}

	protected static TransitivityExplanation scratchExp = new TransitivityExplanation();

	protected TransitivityExplanation createExplanation(
			ReasonedLinkDatabase reasoner, Link temp, Link link, Link gpLink) {
		return new TransitivityExplanation(new OBORestrictionImpl(temp
				.getChild(), temp.getType(), temp.getParent(), true), link,
				gpLink);
	}

	@Override
	public String toString() {
		return "Transitivity rule (implicationTime = "
				+ (implicationTime / 1000000d) + ", expConstTime = "
				+ (expConstructTime / 1000000d) + ", fetchTime = "
				+ (fetchTime / 1000000d) + ")";
	}

	protected void pushLinkDown(ReasonedLinkDatabase reasoner, Link link,
			Collection<Explanation> out, Collection<Link> seenem) {
		if (seenem.contains(link))
			return;
		else
			seenem.add(link);
		Collection<Link> children = reasoner.getChildren(link.getChild());
		for (Link gcLink : children) {
			if (TermUtil.isIntersection(gcLink))
				continue;
			if (ReasonerUtil.generateTransitiveImplication(reasoner, temp,
					gcLink, link)) {
				TransitivityExplanation exp = new TransitivityExplanation(
						new OBORestrictionImpl(temp), gcLink, link);
				if (!ReasonerUtil.containsExplanation(reasoner, exp)) {
					out.add(exp);
					pushLinkDown(reasoner, gcLink, out, seenem);
				}
			}
		}
	}
}
