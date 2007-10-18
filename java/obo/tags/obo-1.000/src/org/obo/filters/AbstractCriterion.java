package org.obo.filters;

/**
 * 
 * A basic abstract implementation of {@link SearchCriterion }
 * 
 */

public abstract class AbstractCriterion<IN_TYPE, OUT_TYPE> implements SearchCriterion<IN_TYPE, OUT_TYPE> {

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
	
	public int getMaxCardinality() {
		return Integer.MAX_VALUE;
	}
	
	public int getMinCardinality() {
		return 0;
	}
}
