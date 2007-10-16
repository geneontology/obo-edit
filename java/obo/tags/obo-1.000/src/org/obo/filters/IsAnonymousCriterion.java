package org.obo.filters;

import org.obo.datamodel.*;

public class IsAnonymousCriterion extends AbstractBooleanCriterion {

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
