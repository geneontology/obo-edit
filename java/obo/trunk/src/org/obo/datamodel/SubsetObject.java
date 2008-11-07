package org.obo.datamodel;

import java.util.Set;

public interface SubsetObject {

	public Set<TermSubset> getSubsets();

	public void addCategory(TermSubset category);

	public void removeCategory(TermSubset category);

	public void addCategoryExtension(TermSubset category, NestedValue nv);

	public NestedValue getCategoryExtension(TermSubset category);
}
