package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

import org.apache.log4j.*;

public class TransitiveOverExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TransitiveOverExplanation.class);
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
