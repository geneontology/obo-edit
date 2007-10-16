package org.obo.datamodel.impl;

import org.obo.datamodel.*;

public class PropertyValueImpl implements PropertyValue {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1555285338106605459L;
	protected String property;
	protected String value;

	public PropertyValueImpl(String property, String value) {
		this.property = property;
		this.value = value;
	}

	public String getValue() {
		return value;
	}

	public String getProperty() {
		return property;
	}

	@Override
	public String toString() {
		return property + "=" + value;
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
}
