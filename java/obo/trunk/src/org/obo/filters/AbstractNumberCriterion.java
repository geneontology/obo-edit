package org.obo.filters;

public abstract class AbstractNumberCriterion<INPUT_TYPE> extends
		AbstractCriterion<INPUT_TYPE, Integer> {

	/*
	 * All extensions of this class return integers, so this method returns
	 * Integer.class
	 * 
	 * @return Integer.class
	 */
	public Class<Integer> getReturnType() {
		return Integer.class;
	}

	public boolean isLegal(String value) {
		try {
			Integer.parseInt(value);
			return true;
		} catch (NumberFormatException ex) {
		}
		return false;
	}
}
