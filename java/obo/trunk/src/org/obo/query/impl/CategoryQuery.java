package org.obo.query.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.TermCategory;
import org.obo.query.Query;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class CategoryQuery implements Query<OBOClass, OBOClass> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CategoryQuery.class);

	protected Collection<String> categoryIDs = new HashSet<String>();

	protected Comparator<OBOClass> comparator = new Comparator<OBOClass>() {
		public int compare(OBOClass o1, OBOClass o2) {
			return o1.getName().compareToIgnoreCase(o2.getName());
		}
	};

	public CategoryQuery(TermCategory... categories) {
		for (TermCategory ns : categories) {
			categoryIDs.add(ns.getName());
		}
	}

	public CategoryQuery(String... categoryNames) {
		for (String ns : categoryNames)
			this.categoryIDs.add(ns);
	}

	public CategoryQuery(Collection<String> categoryNames) {
		this.categoryIDs = categoryNames;
	}

	public OBOClass convertToInputType(OBOClass original) {
		return original;
	}

	public Collection<OBOClass> createResultHolder() {
		return new ArrayList<OBOClass>();
	}

	public void setComparator(Comparator<OBOClass> comparator) {
		this.comparator = comparator;
	}

	public Comparator<OBOClass> getComparator() {
		return comparator;
	}

	public Class<OBOClass> getInputType() {
		return OBOClass.class;
	}

	public OBOClass matches(OBOClass a) {
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
		return o instanceof CategoryQuery
				&& ObjectUtil.equals(((CategoryQuery) o).getCategoryNames(),
						categoryIDs);
	}

	public Collection<String> getCategoryNames() {
		return categoryIDs;
	}

	public OBOClass convertToOutputType(OBOClass original) {
		return original;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return null;
	}

	public void setFieldPath(FieldPath path) {
	}
}
