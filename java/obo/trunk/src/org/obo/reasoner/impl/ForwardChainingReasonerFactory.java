package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

import org.apache.log4j.*;

public class ForwardChainingReasonerFactory implements ReasonerFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ForwardChainingReasonerFactory.class);

	public ForwardChainingReasonerFactory() {
	}
	
	public ReasonedLinkDatabase createReasoner() {
		return new ForwardChainingReasoner();
	}

}
