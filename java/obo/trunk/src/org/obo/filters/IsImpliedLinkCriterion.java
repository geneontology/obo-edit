package org.obo.filters;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class IsImpliedLinkCriterion extends AbstractBooleanLinkCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsImpliedLinkCriterion.class);

	public boolean matches(Link o) {
		return ((Impliable) o).isImplied();
	}

	public String getID() {
		return "is_implied";
	}

	@Override
	public String toString() {
		return "Is implied";
	}
}
