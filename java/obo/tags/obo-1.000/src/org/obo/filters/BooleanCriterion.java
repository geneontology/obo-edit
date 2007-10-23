package org.obo.filters;

public interface BooleanCriterion<IN_TYPE> extends SearchCriterion<IN_TYPE, Boolean> {

	/*
	 * Checks whether an object matches this criterion
	 * 
	 * @param o the object to match against this criterion @return whether or
	 * not the object matches this criterion
	 */
	public boolean matches(IN_TYPE o);

}
