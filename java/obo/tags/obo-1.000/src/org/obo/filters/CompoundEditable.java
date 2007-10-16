package org.obo.filters;

/*
 * Interface for any editing component that handles compound filters.
 */

public interface CompoundEditable extends FilterEditor {

	/*
	 * Sets whether or not the compound filter editor is going to be visible
	 * 
	 * @param showCompoundFilter whether to show the compound filter editor
	 */
	public void setShowCompoundFilter(boolean showCompoundFilter);

	/*
	 * Sets the factory to use for creating new filters
	 * 
	 * @param filterFactory the factory to use to create new filters
	 */
	public void setFilterFactory(FilterFactory filterFactory);
}
