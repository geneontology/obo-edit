package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

public class LinkPileReasonerFactory implements ReasonerFactory {

	public ReasonedLinkDatabase createReasoner() {
		return new LinkPileReasoner();
	}
	
}
