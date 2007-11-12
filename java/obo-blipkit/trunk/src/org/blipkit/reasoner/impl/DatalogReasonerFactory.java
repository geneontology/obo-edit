package org.blipkit.reasoner.impl;

import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;

public class DatalogReasonerFactory implements ReasonerFactory {

	public ReasonedLinkDatabase createReasoner() {
		return new DatalogReasoner();
	}
}

