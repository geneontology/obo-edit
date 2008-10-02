package org.obo.reasoner.rbr;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

import org.apache.log4j.*;

public class LinkCompositionExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkCompositionExplanation.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = -795084980001820300L;

	protected Link link0;
	protected Link link1;

	public LinkCompositionExplanation(Link link0, Link link1) {
		this.link0 = link0;
		this.link1 = link1;
		addEvidence(link0);
		addEvidence(link1);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.HOLDS_OVER_CHAIN;
	}

	@Override
	public String toString() {
		return "HOLDS_OVER_CHAIN: from link " + link0+" . "+link1;
	}
}
