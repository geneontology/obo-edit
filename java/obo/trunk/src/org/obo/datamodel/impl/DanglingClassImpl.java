package org.obo.datamodel.impl;

import java.util.Set;

import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.TermCategory;

public class DanglingClassImpl extends DanglingAnnotatedObjectImpl implements
		OBOClass {

	public DanglingClassImpl(String id) {
		super(id);
	}

	public void addCategory(TermCategory category) {
	}

	public void addCategoryExtension(TermCategory category, NestedValue nv) {
	}

	public Set<TermCategory> getCategories() {
		return null;
	}

	public NestedValue getCategoryExtension(TermCategory category) {
		return null;
	}

	public void removeCategory(TermCategory category) {
	}

	public int compareTo(Object o) {
		return -1;
	}
}
