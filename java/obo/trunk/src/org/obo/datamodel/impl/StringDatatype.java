package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class StringDatatype extends SimpleDatatype<String> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(StringDatatype.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -3023102931769871953L;
	
	public StringDatatype() {
		super("xsd:string");
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public String getComment() {
		return "Represents a string value";
	}

	@Override
	public Namespace getNamespace() {
		return null;
	}

	@Override
	public boolean isAbstract() {
		return false;
	}

	@Override
	public Datatype getSupertype() {
		return Datatype.SIMPLE_TYPE;
	}

	@Override
	public boolean isLegalValue(String string) {
		return string != null;
	}

	
	@Override
	public String getValue(String string) {
		return string;
	}

	@Override
	public String getString(String o) {
		if (o instanceof String)
			return (String) o;
		else
			throw new IllegalArgumentException("StringDatatype can only "
					+ "convert values of type " + "java.lang.String");
	}
}
