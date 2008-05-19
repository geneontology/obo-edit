package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

import org.apache.log4j.*;

public class LinkPileReasonerFactory implements ReasonerFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkPileReasonerFactory.class);

	public ReasonedLinkDatabase createReasoner() {
		return new LinkPileReasoner();
	}
	
}
