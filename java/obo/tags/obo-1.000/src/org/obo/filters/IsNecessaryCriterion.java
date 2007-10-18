package org.obo.filters;

import org.obo.datamodel.*;

public class IsNecessaryCriterion extends AbstractBooleanLinkCriterion {

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
		return "Is Necessary";
	}
}
