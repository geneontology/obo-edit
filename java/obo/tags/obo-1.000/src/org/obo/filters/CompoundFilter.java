package org.obo.filters;

/*
 * A boolean combination of several filters. Filters can be combined via
 * AND or OR. When determining whether a compound filter is true or false,
 * the constituent filters are evaluated in the order they were added.
 * Therefore, it makes sense to add quickly evaluated filters first.
 */

import java.util.*;

public interface CompoundFilter extends Filter {

	public static final int AND = 0;
	public static final int OR = 1;

	/**
	 * 
	 * Cleears out all the sub filters
	 * 
	 */
	public void clear();

	/*
	 * Adds a filter to this CompoundFilter
	 * 
	 * @param f the filter to add
	 */
	public void addFilter(Filter<?> f);

	/*
	 * Removes a filter from this CompoundFilter
	 * 
	 * @param f the filter to remove
	 */
	public void removeFilter(Filter<?> f);

	/*
	 * Returns the list of constituent filters
	 * 
	 * @return the list of constituent filters
	 */
	public List<Filter<?>> getFilters();

	/*
	 * Sets the list of constituent filters. Any previously added constituent
	 * filters will be discarded.
	 * 
	 * @param filters a list of constituent filters to add
	 */
	public void setFilters(List<Filter<?>> filters);

	/*
	 * Sets the boolean operation to use when evaluating this filter. Possible
	 * values are {@link #AND } or {@link #OR }
	 * 
	 * @param booleanOperation the boolean operation to use
	 */
	public void setBooleanOperation(int booleanOperation);

	/*
	 * Returns the current boolean operation
	 * 
	 * @return the current boolean operation
	 */
	public int getBooleanOperation();
}
