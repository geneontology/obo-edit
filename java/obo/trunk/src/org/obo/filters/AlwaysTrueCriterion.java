package org.obo.filters;

import org.obo.datamodel.IdentifiedObject;

import org.apache.log4j.*;

public class AlwaysTrueCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AlwaysTrueCriterion.class);

	public boolean matches(IdentifiedObject o) {
		return true;
	}

	@Override
	public Class getInputType() {
		return Object.class;
	}

	public String getID() {
		return "always_true";
	}

	@Override
	public String toString() {
		return "is always true";
	}
}
