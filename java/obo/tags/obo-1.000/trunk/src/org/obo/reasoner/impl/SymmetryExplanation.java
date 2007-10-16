package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class SymmetryExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6029631779179877969L;

	public SymmetryExplanation(Link original, Link link) {
		setExplainedLink(link);
		addEvidence(original);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.SYMMETRY;
	}
	
	@Override
	public String toString() {
		return "SYMMETRY: from link " + getExplainedLink();
	}
}
