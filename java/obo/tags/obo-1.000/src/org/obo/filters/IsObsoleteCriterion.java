package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

public class IsObsoleteCriterion extends AbstractBooleanCriterion {

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
