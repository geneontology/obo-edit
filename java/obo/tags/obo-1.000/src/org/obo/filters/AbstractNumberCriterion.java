package org.obo.filters;

public abstract class AbstractNumberCriterion extends AbstractCriterion {

	/*
	 * All extensions of this class return integers, so this method returns
	 * Integer.class
	 * 
	 * @return Integer.class
	 */
	public Class getReturnType() {
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
