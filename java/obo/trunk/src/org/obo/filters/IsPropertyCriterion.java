package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class IsPropertyCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsPropertyCriterion.class);

	public String getID() {
		return "is_property";
	}

	public boolean matches(IdentifiedObject o) {
		return TermUtil.isProperty(o);
	}

	@Override
	public String toString() {
		return "Is Property";
	}
}
