package org.obo.reasoner.rbr;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

import org.apache.log4j.*;

public class AlwaysImpliesInverseExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AlwaysImpliesInverseExplanation.class);
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
