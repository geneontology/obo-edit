package org.obo.filters;

/*
 * This interface is used to choose a collection of values
 * (usually corresponding to the values of some property) from an object.
 * 
 * OBO-Edit's search system uses a {@link SearchAspect },
 * {@link SearchComparison} and a value to check against to search the terms
 * and links in an ontology.
 *
 * Some SearchCriterion do not return a collection of values, but instead
 * return a single boolean value. Those search criteria implement
 * {@link BooleanCriterion }, and must implement
 * the {@link BooleanCriterion#matches(Object) } method.
 */

import java.util.Collection;

public interface SearchCriterion<IN_TYPE, OUT_TYPE> {

	/*
	 * Returns a collection of values from an object (usually corresponding to a
	 * property of the object). In order to avoid the need to create lots of
	 * collection objects, a scratch collection is passed in that can be
	 * populated and returned by this method.
	 * 
	 * @param scratch a preinstatiated collection that can be populated with
	 * values and returned @param obj the object from which the values are
	 * drawn; the potential values for this parameter are constrained by
	 * {@link #getInputType() } @return a collection of values. The items in
	 * this collection must be of the same class, specified by
	 * {@link #getReturnType() }
	 */
	public Collection<OUT_TYPE> getValues(Collection<OUT_TYPE> scratch, IN_TYPE obj);

	/*
	 * Returns the input type that is allowable for the
	 * {@link #getValues(Collection, Object) } method.
	 * 
	 * @return the class of object usable by
	 * {@link getValues(Collection, Object) }
	 */
	public Class<IN_TYPE> getInputType();

	/*
	 * Returns the type of the objects in the collection return by
	 * {@link #getValues(Collection, Object) }
	 * 
	 * @return the class of object wrapped in the collection returned by
	 * {@link #getValues(Collection, Object) }
	 */
	public Class<OUT_TYPE> getReturnType();

	/*
	 * Returns the id of this SearchCriterion.
	 * 
	 * @return the id of this criterion
	 */
	public String getID();
	
	public int getMinCardinality();
	
	public int getMaxCardinality(); 

}
