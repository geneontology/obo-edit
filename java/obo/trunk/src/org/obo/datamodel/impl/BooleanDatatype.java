package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class BooleanDatatype extends SimpleDatatype<Boolean> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BooleanDatatype.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 4670390380971503460L;

	@Override
	public String getID() {
		return "xsd:boolean";
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public String getComment() {
		return "Represents a boolean (true/false) value";
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
		return string.equals("true") || string.equals("false");
	}

	@Override
	public Boolean getValue(String string) {
		return new Boolean(string);
	}

	@Override
	public String getString(Boolean o) {
		return o.toString();
	}
}
