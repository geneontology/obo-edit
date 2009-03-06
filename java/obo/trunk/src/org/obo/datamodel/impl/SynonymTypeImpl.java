package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;
import org.bbop.util.ObjectUtil;

public class SynonymTypeImpl implements SynonymType {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymTypeImpl.class);

	protected String id;
	protected String name;
	protected int scope;

	public SynonymTypeImpl() {
		this(null, null, Synonym.UNKNOWN_SCOPE);
	}

	public SynonymTypeImpl(String id, String name) {
		this(id, name, Synonym.UNKNOWN_SCOPE);
	}

	public SynonymTypeImpl(String id, String name, int scope) {
		setID(id);
		setName(name);
		setScope(scope);
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			return null;
		}
	}

	public void setID(String id) {
		this.id = id;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setScope(int scope) {
		this.scope = scope;
	}

	public String getID() {
		return id;
	}

	public String getName() {
		return name;
	}

	public int getScope() {
		return scope;
	}

	@Override
	public String toString() {
		return name;
	}

	@Override
	public int hashCode() {
		return id.hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof SynonymType) {
			return ObjectUtil.equals(((SynonymType) o).getID(), id);
		} else
			return false;
	}
}
