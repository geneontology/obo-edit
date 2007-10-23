package org.obo.filters;

import org.obo.datamodel.*;

public class IsImpliedLinkCriterion extends AbstractBooleanLinkCriterion {

	public boolean matches(Link o) {
		if (o instanceof Impliable) {
			return ((Impliable) o).isImplied();
		} else
			return false;
	}

	public String getID() {
		return "is_implied";
	}

	@Override
	public String toString() {
		return "Is implied";
	}
}
