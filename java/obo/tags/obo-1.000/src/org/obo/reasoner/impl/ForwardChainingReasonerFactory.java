package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

public class ForwardChainingReasonerFactory implements ReasonerFactory {

	public ReasonedLinkDatabase createReasoner() {
		return new ForwardChainingReasoner();
	}

}
