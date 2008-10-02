package org.obo.reasoner.rbr;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

import org.apache.log4j.*;

public class GivenExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GivenExplanation.class);

	public GivenExplanation(Link link) {
		setExplainedLink(link);
	}
	
	public ExplanationType getExplanationType() {
		return ExplanationType.GIVEN;
	}
	
	public String toString() {
		return "GIVEN: "+getExplainedLink();
	}

}
