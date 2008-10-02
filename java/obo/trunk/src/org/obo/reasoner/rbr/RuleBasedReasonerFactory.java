package org.obo.reasoner.rbr;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

import org.apache.log4j.*;

public class RuleBasedReasonerFactory implements ReasonerFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RuleBasedReasonerFactory.class);

	public ReasonedLinkDatabase createReasoner() {
		return new RuleBasedReasoner();
	}
	
}
