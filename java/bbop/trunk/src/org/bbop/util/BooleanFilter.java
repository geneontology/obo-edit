package org.bbop.util;

import java.util.*;

/**
 * A VectorFilter that allows other VectorFilters to be grouped together via
 * boolean expressions.
 * @see org.bbop.util.VectorUtil#filter(VectorFilter, Vector)
 */
import org.apache.log4j.*;

public class BooleanFilter implements VectorFilter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BooleanFilter.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = -3102761890208817994L;
	public static final int AND = 1;
    public static final int OR = 2;
    public static final int NOT = 3;

    protected int operation;
    protected List filters;

    public BooleanFilter() {
	this.filters = new ArrayList();
	operation = AND;
    }

    /**
     * Constructs a new BooleanFilter with the specified boolean operator
     * @param operation the operator to use, either AND, OR, or NOT
     */
    public BooleanFilter(int operation) {
	setOperation(operation);
	this.filters = new ArrayList();
    }

   /**
    * Removes all installed filters.
    */
    public void removeAll() {
	filters.clear();
    }

    public Object clone() {
	try {
	    return super.clone();
	} catch (Exception e) {
	    return null;
	}
    }

    /**
     * Returns the current operation: AND, OR, or NOT
     */

    public int getOperation() {
	return operation;
    }

    /**
     * Sets the current operation. Must be one of AND, OR or NOT
     */

    public void setOperation(int operation) {
	if (operation != AND &&
	    operation != OR &&
	    operation != NOT)
	    throw new IllegalArgumentException("Illegal Operation");
	this.operation = operation;
    }

    /**
     * Adds a new filter to this BooleanFilter. If this is a NOT filter,
     * only one filter can be added. Subsequent calls to this method do nothing
     * until the first filter is removed.
     */
    public void addFilter(VectorFilter in) {
	if (!filters.contains(in)) {
	    if (filters.size() < 1 || (operation != NOT))
		filters.add(in);
	}
    }

    /**
     * Removes the specified filter
     */
    public void removeFilter(VectorFilter in) {
	filters.remove(in);
    }

   /**
    * Returns the list of filters
    * @return the list of filters
    */
    public List getFilters() {
	return filters;
    }

    public void setFilters(List filters) {
	this.filters = filters;
    }

    /**
     * If the operation for this filter is AND, this method returns the
     * result of each of the satisfies() methods of the child filters ANDed
     * together. If the operation is or, this method returns the result of
     * the child filters ORed together. If the operation is NOT, this method
     * returns the opposite of the satisfies() method of the child filter.
     * @return the application of the specified boolean operation to the
     *         results of each of the child filters
     * @param o an object
     */
    public boolean satisfies(Object o) {
	Iterator it = filters.iterator();
	while(it.hasNext()) {
	    VectorFilter current = (VectorFilter) it.next();
	    boolean result = current.satisfies(o);
	    if (operation == NOT)
		return !result;
	    else if (operation == OR && result)
		return true;
	    else if (operation == AND && !result)
		return false;
	}
	if (operation == OR)
	    return false;
	else if (operation == AND)
	    return true;
	else
	    return false;
    }
}
