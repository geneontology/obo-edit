package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

import org.apache.log4j.*;

public class HoldsOverChainExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HoldsOverChainExplanation.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = -795084980001820300L;

	protected Link link;

	public HoldsOverChainExplanation(Link link) {
		this.link = link;
		addEvidence(link);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.HOLDS_OVER_CHAIN;
	}

	@Override
	public String toString() {
		return "HOLDS_OVER_CHAIN: from link " + link;
	}
}
