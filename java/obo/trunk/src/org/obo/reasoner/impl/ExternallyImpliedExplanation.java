package org.obo.reasoner.impl;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class ExternallyImpliedExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6029631779179877969L;
	
	protected String message;
	
	public ExternallyImpliedExplanation() {
		this(null, null);
	}
	
	public ExternallyImpliedExplanation(Link explained) {
		this(null, explained);
	}

	public ExternallyImpliedExplanation(String message, Link explained) {
		this.message = message;
		setExplainedLink(explained);
	}
	
	public String getMessage() {
		return message;
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.IMPLIED_BY_EXTERNAL_REASONER;
	}
	
	@Override
	public String toString() {
		return "EXTERNALLY IMPLIED";
	}
}
