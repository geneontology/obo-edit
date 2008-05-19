package org.obo.filters;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class IsBuiltinCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsBuiltinCriterion.class);

	public String getID() {
		return "is_builtin";
	}

	public boolean matches(IdentifiedObject o) {
		return ((IdentifiedObject) o).isBuiltIn();
	}

	@Override
	public String toString() {
		return "Is Built In";
	}
}
