package org.obo.filters;

import org.obo.datamodel.*;

public class IsImpliedLinkCriterion extends AbstractBooleanLinkCriterion {

	public boolean matches(Link o) {
		return ((Impliable) o).isImplied();
	}

	public String getID() {
		return "is_implied";
	}

	@Override
	public String toString() {
		return "Is implied";
	}
}
