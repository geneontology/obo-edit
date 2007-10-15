package org.obo.filters;

import org.obo.datamodel.IdentifiedObject;

public class AlwaysTrueCriterion extends AbstractBooleanCriterion {

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
