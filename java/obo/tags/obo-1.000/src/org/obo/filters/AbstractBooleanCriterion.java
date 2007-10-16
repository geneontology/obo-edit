package org.obo.filters;

/**
 *
 * An instance of SearchCriterion that can be used as a framework to build
 * boolean search criteria
 *
 */

import java.util.Collection;

import org.obo.datamodel.*;

public abstract class AbstractBooleanCriterion extends
		AbstractCriterion<IdentifiedObject, Boolean> implements
		BooleanCriterion<IdentifiedObject> {

	public Class<Boolean> getReturnType() {
		return Boolean.class;
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	public Collection<Boolean> getValues(Collection<Boolean> scratch,
			IdentifiedObject obj) {
		if (matches(obj))
			scratch.add(Boolean.TRUE);
		else
			scratch.add(Boolean.FALSE);

		return scratch;
	}
	
	public int getMaxCardinality() {
		return 1;
	}

	public int getMinCardinality() {
		return 1;
	}

	public boolean isLegal(String value) {
		return true;
	}
}
