package org.obo.filters;

import org.obo.datamodel.LinkDatabase;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.TrimmedLinkDatabase;

/**
 * 
 * A basic abstract implementation of {@link SearchCriterion }
 * 
 */

public abstract class AbstractCriterion<IN_TYPE, OUT_TYPE> implements SearchCriterion<IN_TYPE, OUT_TYPE> {

	protected ReasonedLinkDatabase reasoner;
	protected LinkDatabase trimmedReasoner;
	
	/**
	 * Matches any SearchCriterion of the same class
	 */
	@Override
	public boolean equals(Object o) {
		return o instanceof SearchCriterion && getClass().equals(o.getClass());
	}

	/*
	 * The hashcode is based on the class
	 * 
	 * @return getClass().hashCode()
	 */
	@Override
	public int hashCode() {
		return getClass().hashCode();
	}
	
	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
		if (reasoner == null)
			trimmedReasoner = null;
		else
			trimmedReasoner = new TrimmedLinkDatabase(reasoner);
	}
	
	public int getMaxCardinality() {
		return Integer.MAX_VALUE;
	}
	
	public int getMinCardinality() {
		return 0;
	}
}
