package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

public class IsCompleteLinkCriterion extends AbstractBooleanLinkCriterion {

	public boolean matches(Link o) {
		return TermUtil.isIntersection((Link) o);
	}

	@Override
	public Class<Link> getInputType() {
		return Link.class;
	}

	public String getID() {
		return "is_intersection_link";
	}

	@Override
	public String toString() {
		return "Is intersection";
	}
}
