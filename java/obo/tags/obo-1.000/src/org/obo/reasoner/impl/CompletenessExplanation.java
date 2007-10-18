package org.obo.reasoner.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ExplanationType;

public class CompletenessExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6982440827960096291L;

	protected Collection<CompletenessMatch> matches = new LinkedList<CompletenessMatch>();
	protected Collection<Link> matchDefinition;

	public CompletenessExplanation() {
	}
	
	public void setMatchDefinition(Collection<Link> matchDefinition) {
		this.matchDefinition = matchDefinition;
	}

	public Collection<CompletenessMatch> getMatches() {
		return matches;
	}

	public void addMatch(CompletenessMatch match) {
		if (!matches.contains(match))
			matches.add(match);
		addEvidence(match.getMatchLink());
		addEvidence(match.getCompletenessLink());
	}

	protected void removeMatch(CompletenessMatch match) {
		matches.remove(match);
		super.removeEvidence(match.getMatchLink());
		super.removeEvidence(match.getCompletenessLink());
	}
	
	public boolean removeEvidence(Link link) {
		for (CompletenessMatch match : matches) {
			if (match.getMatchLink().equals(link)
					|| match.getCompletenessLink().equals(link)) {
				removeMatch(match);
				break;
			}
		}
		boolean broken = !isSupported();
		return broken;
	}

	protected boolean isSupported() {
		for(Link link : matchDefinition) {
			boolean found = false;
			for(CompletenessMatch match : matches) {
				if (match.getCompletenessLink().equals(link)) {
					found = true;
				}
			}
			if (!found)
				return false;
		}
		return true;
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.INTERSECTION;
	}

	@Override
	public String toString() {
		return "COMPLETENESS: matches=" + matches;
	}
}
