package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

public class IsMarkedAsInferredLinkCriterion extends AbstractBooleanLinkCriterion {

	public boolean matches(Link link) {
		return TermUtil.isMarkedAsInferred(link);
	}

	public String getID() {
		return "is_marked_as_inferred";
	}

	@Override
	public String toString() {
		return "Is marked as inferred";
	}
}
