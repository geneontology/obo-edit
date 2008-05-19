package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class IsObsoleteCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsObsoleteCriterion.class);

	public String getID() {
		return "is_obsolete";
	}

	public boolean matches(IdentifiedObject o) {
		return TermUtil.isObsolete(o);
	}

	@Override
	public String toString() {
		return "Is Obsolete";
	}
}
