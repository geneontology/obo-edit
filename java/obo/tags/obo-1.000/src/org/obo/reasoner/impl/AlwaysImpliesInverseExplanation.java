package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class AlwaysImpliesInverseExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6029631779179877969L;
	protected Link link;

	public AlwaysImpliesInverseExplanation(Link link, Link original) {
		this.link = link;
		addEvidence(original);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.INVERSION;
	}
	
	@Override
	public String toString() {
		return "ALWAYS_IMPLIES_INVERSE: from link " + link;
	}
}
