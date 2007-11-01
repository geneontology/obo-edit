package org.obo.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

public class PelletWrappedReasonerFactory implements ReasonerFactory {

	public ReasonedLinkDatabase createReasoner() {
		return new PelletWrappedReasoner();
	}
}

