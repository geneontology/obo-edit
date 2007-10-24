package org.obo.filters;

/*
 * A filter factory that builds {@link CompoundFilter}s.
 *
 */

public class CompoundFilterFactory implements FilterFactory {

	/*
	 * Creates a new {@link CompoundFilterImpl}
	 * 
	 * @return a new compound filter
	 */
	public Filter createNewFilter() {
		return new CompoundFilterImpl();
	}
}
