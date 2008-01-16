package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class TransitiveOverExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = -795084980001820300L;

	protected Link link;

	public TransitiveOverExplanation(Link link) {
		this.link = link;
		addEvidence(link);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.TRANSITIVE_OVER;
	}

	@Override
	public String toString() {
		return "TRANSITIVE_OVER: from intersection link " + link;
	}
}
