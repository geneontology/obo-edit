package org.obo.query.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.TermCategory;
import org.obo.query.Query;
import org.obo.util.TermUtil;

/** 
    This should replace CategoryQuery.
    Cateogries are also called slims - and should probably be renamed?
    This queries by slim, and returns generic OBOObjects
    (CategoryQuery return OBOClasses)
 */


public class CategoryObjQuery implements Query<OBOObject, OBOObject> {

	protected Collection<String> categoryIDs = new HashSet<String>();

	protected Comparator<OBOObject> comparator = new Comparator<OBOObject>() {
		public int compare(OBOObject o1, OBOObject o2) {
			return o1.getName().compareToIgnoreCase(o2.getName());
		}
	};

	public CategoryObjQuery(TermCategory... categories) {
		for (TermCategory ns : categories) {
			categoryIDs.add(ns.getName());
		}
	}

	public CategoryObjQuery(String... categoryNames) {
		for (String ns : categoryNames)
			this.categoryIDs.add(ns);
	}

	public CategoryObjQuery(Collection<String> categoryNames) {
		this.categoryIDs = categoryNames;
	}

	public OBOObject convertToInputType(OBOObject original) {
		return original;
	}

	public Collection<OBOObject> createResultHolder() {
		return new ArrayList<OBOObject>();
	}

	public void setComparator(Comparator<OBOObject> comparator) {
		this.comparator = comparator;
	}

	public Comparator<OBOObject> getComparator() {
		return comparator;
	}

	public Class<OBOObject> getInputType() {
		return OBOObject.class;
	}

	public OBOObject matches(OBOObject a) {
		if (!a.isBuiltIn() && TermUtil.isClass(a)) {
			for (String catName : categoryIDs) {
				for (TermCategory cat : a.getCategories()) {
					if (cat.getName().equals(catName))
						return a;
				}
			}
		}
		return null;
	}

	public boolean equals(Object o) {
		return o instanceof CategoryObjQuery
				&& ObjectUtil.equals(((CategoryObjQuery) o).getCategoryNames(),
						categoryIDs);
	}

	public Collection<String> getCategoryNames() {
		return categoryIDs;
	}

	public OBOObject convertToOutputType(OBOObject original) {
		return original;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return null;
	}

	public void setFieldPath(FieldPath path) {
	}
}
