package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

public class IsClassCriterion extends AbstractBooleanCriterion {

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
