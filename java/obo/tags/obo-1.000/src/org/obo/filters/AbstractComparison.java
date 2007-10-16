package org.obo.filters;

/**
 * 
 * An abstract implementation of search comparison that provides basic
 * implementations of init(), cleanup(), equals(), hashCode(), and toString()
 * 
 */

public abstract class AbstractComparison implements SearchComparison {

	/*
	 * Does nothing
	 */
	public void init() {
	}

	/*
	 * Does nothing
	 */
	public void cleanup() {
	}

	/*
	 * An implementation of abstract search comparison equals another search
	 * comparison if they are of the same class
	 */
	@Override
	public boolean equals(Object o) {
		return o instanceof SearchComparison && getClass().equals(o.getClass());
	}

	/*
	 * Since search comparisons are singleton classes, return a hash code based
	 * on the class.
	 * 
	 * @return getClass().hashCode()
	 */
	@Override
	public int hashCode() {
		return getClass().hashCode();
	}

	/*
	 * The default toString() implementation returns the comparison id
	 * 
	 * @return a string based on the id
	 */
	@Override
	public String toString() {
		return getID();
	}
}
