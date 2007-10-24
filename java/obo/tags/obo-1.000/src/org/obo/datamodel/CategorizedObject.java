package org.obo.datamodel;

import java.util.Set;

public interface CategorizedObject {

	public Set<TermCategory> getCategories();

	public void addCategory(TermCategory category);

	public void removeCategory(TermCategory category);

	public void addCategoryExtension(TermCategory category, NestedValue nv);

	public NestedValue getCategoryExtension(TermCategory category);
}
