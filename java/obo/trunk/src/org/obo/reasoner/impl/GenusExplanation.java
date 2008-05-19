package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

import org.apache.log4j.*;

public class GenusExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GenusExplanation.class);
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
