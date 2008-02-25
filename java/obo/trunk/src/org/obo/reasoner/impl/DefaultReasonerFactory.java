package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

/**
 * We use {@link LinkPileReasoner} by default  
 * @author cjm
 *
 */
public class DefaultReasonerFactory implements ReasonerFactory {

	public ReasonedLinkDatabase createReasoner() {
		return new LinkPileReasoner();
	}
	
}
