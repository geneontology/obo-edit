package org.obo.filters;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class IsAnonymousCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsAnonymousCriterion.class);

	public String getID() {
		return "is_anonymous";
	}

	public boolean matches(IdentifiedObject o) {
		return ((IdentifiedObject) o).isAnonymous();
	}

	@Override
	public String toString() {
		return "Is anonymous";
	}
}
