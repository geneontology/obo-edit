package org.obo.reasoner.impl;

import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

public abstract class AbstractReasonerRule implements ReasonerRule {

	public long ruleTime;
	protected boolean allowIntersections = false;

	public void setAllowIntersections(boolean allowIntersections) {
		this.allowIntersections = allowIntersections;
	}

	public void end(ReasonedLinkDatabase reasoner) {

	}

	public void init(ReasonedLinkDatabase reasoner) {
		ruleTime = 0;
	}

	public void install(ReasonedLinkDatabase reasoner) {

	}

	public void uninstall(ReasonedLinkDatabase reasoner) {

	}

	/**
	 * on adding a new link, check for entailments
	 * @param reasoner
	 * @param newLink
	 * @return
	 */
	protected abstract Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink);

	public Collection<Explanation> getImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
		/*
		 * normally we ignore intersection (N+S) links as they are trivially redundant with
		 * the equivalent normal (N) link, which is always inferred by GenusDifferentiaRule.
		 * The exception is GenusDifferentiaRule itself, which infers the normal link.
		 * 
		 * This appears to be the sole point of having getImplications delegate to doGetImplications...
		 * [CJM]
		 */
		if (!allowIntersections && TermUtil.isIntersection(newLink))
			return null;
		long time = System.nanoTime();
		Collection<Explanation> out = doGetImplications(reasoner, newLink);
		ruleTime += System.nanoTime() - time;
		return out;
	}

	protected Link createLink(LinkedObject child, OBOProperty type,
			LinkedObject parent) {
		return new LinkPileReasoner.ReasonerLink(child, type, parent);
	}
}
