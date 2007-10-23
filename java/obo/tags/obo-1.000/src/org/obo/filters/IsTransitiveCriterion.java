package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

public class IsTransitiveCriterion extends AbstractBooleanCriterion {

	public String getID() {
		return "is_transitive";
	}

	public boolean matches(IdentifiedObject o) {
		return TermUtil.isProperty(o)
				&& ((OBOProperty) o).isTransitive();
	}

	@Override
	public Class getInputType() {
		return OBOProperty.class;
	}

	@Override
	public String toString() {
		return "Is Transitive";
	}
}
