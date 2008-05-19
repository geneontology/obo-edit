package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class CompletenessMatch {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CompletenessMatch.class);
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
