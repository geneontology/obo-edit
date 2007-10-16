package org.obo.filters;

import org.obo.datamodel.*;

public class IsBuiltinCriterion extends AbstractBooleanCriterion {

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
