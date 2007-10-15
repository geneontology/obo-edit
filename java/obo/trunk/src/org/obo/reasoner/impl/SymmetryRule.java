package org.obo.reasoner.impl;

import java.util.Collection;
import java.util.Collections;

import org.obo.datamodel.Link;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;

public class SymmetryRule extends AbstractReasonerRule {

	public Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link link) {
		if (link.getType().isSymmetric()) {
			Link newLink = new OBORestrictionImpl(link.getParent(), link
					.getType(), link.getChild(), true);
			Explanation exp = new SymmetryExplanation(link, newLink);
			return Collections.singleton(exp);
		} else
			return null;
	}

}
