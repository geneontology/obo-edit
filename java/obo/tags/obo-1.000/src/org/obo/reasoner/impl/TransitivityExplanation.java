package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class TransitivityExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = -4952593548062292144L;
	protected Link directLink;
	protected Link extensionLink;
	
	public TransitivityExplanation(Link explainedLink, Link directLink, Link extensionLink) {
		this(directLink, extensionLink);
		setExplainedLink(explainedLink);
	}
	
	public TransitivityExplanation() {}

	public TransitivityExplanation(Link directLink, Link extensionLink) {
		if (!directLink.getParent().equals(extensionLink.getChild()))
			throw new IllegalArgumentException();
		this.directLink = directLink;
		this.extensionLink = extensionLink;
		addEvidence(directLink);
		addEvidence(extensionLink);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.TRANSITIVITY;
	}

	@Override
	public String toString() {
		return "TRANSITIVITY: "+explainedLink+" from " + extensionLink + " over "
				+ directLink;
	}
}
