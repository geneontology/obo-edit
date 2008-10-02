package org.obo.reasoner.rbr;

import org.obo.datamodel.Link;

/**
 * A CompletenessMatch supports a CompletenessExplanation.
 * A CompletenessExplanation has two or more CompletenessMatches, one for each N+S condition (ie completeness link)
 * @author cjm
 *
 */
public class IntersectionMatch {

	//initialize logger
	protected Link matchLink;
	protected Link completenessLink;

	public IntersectionMatch(Link matchLink, Link completenessLink) {
		this.matchLink = matchLink;
		this.completenessLink = completenessLink;
	}

	@Override
	public String toString() {
		return matchLink + " matches " + completenessLink;
	}

	/**
	 * @return the link that satisfies the N+S condition
	 */
	public Link getMatchLink() {
		return matchLink;
	}

	/**
	 * @return the N+S condition that the link satisfies
	 */
	public Link getCompletenessLink() {
		return completenessLink;
	}
}