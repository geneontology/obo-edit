package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.util.TermUtil;

public class CompletenessMatch {
	protected Link matchLink;
	protected Link completenessLink;

	public CompletenessMatch(Link matchLink, Link completenessLink) {
		this.matchLink = matchLink;
		this.completenessLink = completenessLink;
	}

	@Override
	public String toString() {
		return matchLink + " matches " + completenessLink;
	}

	public Link getMatchLink() {
		return matchLink;
	}

	public Link getCompletenessLink() {
		return completenessLink;
	}
}
