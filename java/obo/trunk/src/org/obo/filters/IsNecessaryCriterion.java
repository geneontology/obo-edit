package org.obo.filters;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class IsNecessaryCriterion extends AbstractBooleanLinkCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsNecessaryCriterion.class);

	public boolean matches(Link o) {
		if (o instanceof OBORestriction) {
			OBORestriction restriction = (OBORestriction) o;
			return restriction.isNecessarilyTrue();
		} else
			return false;
	}

	public String getID() {
		return "is_necessary";
	}

	@Override
	public Class<Link> getInputType() {
		return Link.class;
	}

	@Override
	public String toString() {
		return "Is necessary";
	}
}
