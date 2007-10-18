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

	protected abstract Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink);

	public Collection<Explanation> getImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
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
