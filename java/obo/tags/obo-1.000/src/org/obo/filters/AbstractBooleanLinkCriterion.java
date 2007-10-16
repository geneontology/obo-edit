package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Link;

public abstract class AbstractBooleanLinkCriterion extends
		AbstractCriterion<Link, Boolean> implements BooleanCriterion<Link> {

	public Class<Boolean> getReturnType() {
		return Boolean.class;
	}

	public Class<Link> getInputType() {
		return Link.class;
	}

	public Collection<Boolean> getValues(Collection<Boolean> scratch, Link obj) {
		if (matches(obj))
			scratch.add(Boolean.TRUE);
		else
			scratch.add(Boolean.FALSE);

		return scratch;
	}

	public boolean isLegal(String value) {
		return true;
	}
}
