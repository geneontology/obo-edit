package org.obo.filters;

import org.obo.datamodel.Link;
import org.obo.reasoner.ReasonedLinkDatabase;

public class IsRedundantLinkCriterion extends AbstractBooleanLinkCriterion {

	public static final IsRedundantLinkCriterion CRITERION =
		new IsRedundantLinkCriterion();

	protected ReasonedLinkDatabase reasoner;

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}

	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	public boolean matches(Link o) {
		if (o instanceof Link && getReasoner() != null) {
			Link link = (Link) o;
			return getReasoner().isRedundant(link);
		} else
			return false;
	}

	public String getID() {
		return "is_redundant";
	}

	@Override
	public String toString() {
		return "Is redundant";
	}
}
