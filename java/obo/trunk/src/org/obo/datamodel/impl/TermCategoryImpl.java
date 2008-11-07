package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class TermCategoryImpl implements TermSubset {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermCategoryImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -7390141440212294915L;
	protected String name;
	protected String desc;

	public TermCategoryImpl() {
		this(null, null);
	}

	public TermCategoryImpl(String name, String desc) {
		this.name = name;
		this.desc = desc;
	}

	public String getName() {
		return name;
	}

	public String getDesc() {
		return desc;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setDesc(String desc) {
		this.desc = desc;
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}

	@Override
	public String toString() {
		return desc + " (" + name + ")";
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof TermSubset) {
			return ((TermSubset) o).getName().equals(name);
		} else
			return false;
	}
}
