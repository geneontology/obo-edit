package org.obo.datamodel.impl;

import java.util.Set;

import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.TermSubset;
import org.obo.datamodel.Type;

import org.apache.log4j.*;

public class DanglingClassImpl extends DanglingAnnotatedObjectImpl implements
	OBOClass {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DanglingClassImpl.class);

	public DanglingClassImpl(String id) {
		super(id);
	}
	
	@Override
	public Type<OBOClass> getType() {
		return OBO_CLASS;
	}

	public void addCategory(TermSubset category) {
	}

	public void addCategoryExtension(TermSubset category, NestedValue nv) {
	}

	public Set<TermSubset> getSubsets() {
		return null;
	}

	public NestedValue getCategoryExtension(TermSubset category) {
		return null;
	}

	public void removeCategory(TermSubset category) {
	}

	public int compareTo(Object o) {
		return -1;
	}
}
