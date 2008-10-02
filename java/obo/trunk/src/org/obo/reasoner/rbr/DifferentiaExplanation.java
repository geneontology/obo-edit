package org.obo.reasoner.rbr;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

import org.apache.log4j.*;

public class DifferentiaExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DifferentiaExplanation.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 6029631779179877969L;
	protected Link link;

	public DifferentiaExplanation(Link link) {
		this.link = link;
		addEvidence(link);
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.DIFFERENTIA;
	}
	
	@Override
	public String toString() {
		return "DIFFERENTIA: from intersection link " + link;
	}
}
