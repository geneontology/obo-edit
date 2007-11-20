package org.obo.datamodel.impl;

import org.obo.datamodel.*;

public class PropertyValueImpl implements PropertyValue {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1555285338106605459L;
	protected String property;
	protected String value;
	protected int lineNumber;
	protected String filename;
	
	public PropertyValueImpl(String property, String value) {
		this(property, value, null, -1);
	}

	public PropertyValueImpl(String property, String value, String filename, int lineNumber) {
		this.property = property;
		this.value = value;
		this.lineNumber = lineNumber;
		this.filename = filename;
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

	public int getLineNumber() {
		return lineNumber;
	}

	public String getFilename() {
		return filename;
	}
	
	public String getLine() {
		return property+": "+value;
	}
}
