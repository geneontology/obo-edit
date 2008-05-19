package org.obo.datamodel.impl;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class NestedValueImpl implements NestedValue {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NestedValueImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -7529450225162773796L;
	protected Set propertyValues = new HashSet();
	protected String name;
	protected String suggestedComment;

	public NestedValueImpl() {
	}

	public String getName() {
		return name;
	}

	public Set getPropertyValues() {
		return propertyValues;
	}

	public void addPropertyValue(PropertyValue pv) {
		propertyValues.add(pv);
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException ex) {
			// this will never happen
			return null;
		}
	}

	public String getSuggestedComment() {
		return suggestedComment;
	}

	public void setSuggestedComment(String suggestedComment) {
		this.suggestedComment = suggestedComment;		
	}
}
