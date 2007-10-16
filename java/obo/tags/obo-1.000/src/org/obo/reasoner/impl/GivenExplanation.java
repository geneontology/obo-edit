package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class GivenExplanation extends AbstractExplanation {

	public GivenExplanation(Link link) {
		setExplainedLink(link);
	}
	
	public ExplanationType getExplanationType() {
		return ExplanationType.GIVEN;
	}

}
