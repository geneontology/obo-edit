package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

public class IsPropertyCriterion extends AbstractBooleanCriterion {

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
