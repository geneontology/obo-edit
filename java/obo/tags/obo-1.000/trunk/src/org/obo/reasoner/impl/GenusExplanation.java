package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class GenusExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = -795084980001820300L;

	protected Link link;

	public GenusExplanation(Link link) {
		this.link = link;
		addEvidence(link);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.GENUS;
	}

	@Override
	public String toString() {
		return "GENUS: from intersection link " + link;
	}
}
