package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

import org.apache.log4j.*;

public class PelletWrappedReasonerFactory implements ReasonerFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PelletWrappedReasonerFactory.class);

	public ReasonedLinkDatabase createReasoner() {
		return new PelletWrappedReasoner();
	}
}

