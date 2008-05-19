package org.obo.reasoner.impl;

import java.util.Collection;
import java.util.Collections;

import org.obo.datamodel.Link;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;

import org.apache.log4j.*;

public class SymmetryRule extends AbstractReasonerRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SymmetryRule.class);

	public Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link link) {
		if (link.getType().isSymmetric()) {
			Link newLink = createLink(link.getParent(), link
					.getType(), link.getChild());
			Explanation exp = new SymmetryExplanation(link, newLink);
			return Collections.singleton(exp);
		} else
			return null;
	}

}
