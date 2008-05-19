package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

/**
 * We use {@link LinkPileReasoner} by default  
 * @author cjm
 *
 */
import org.apache.log4j.*;

public class DefaultReasonerFactory implements ReasonerFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultReasonerFactory.class);

	public ReasonedLinkDatabase createReasoner() {
		return new LinkPileReasoner();
	}
	
}
