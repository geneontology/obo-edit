package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class IsClassCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsClassCriterion.class);

	public String getID() {
		return "is_class";
	}

	public boolean matches(IdentifiedObject o) {
		return TermUtil.isClass(o);
	}

	@Override
	public String toString() {
		return "Is Class";
	}
}
