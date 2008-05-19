package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class IntegerDatatype extends SimpleDatatype<Integer> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntegerDatatype.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 7915107256768335522L;

	@Override
	public String getID() {
		return "xsd:integer";
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public String getComment() {
		return "Represents an integer";
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
		if (string == null)
			return false;
		try {
			Integer.parseInt(string);
			return true;
		} catch (NumberFormatException ex) {
			return false;
		}
	}

	@Override
	public Integer getValue(String string) {
		if (string == null)
			return null;
		try {
			return new Integer(string);
		} catch (NumberFormatException ex) {
			throw new IllegalArgumentException("Illegal integer value "
					+ string);
		}
	}

	@Override
	public String getString(Integer o) {
		if (o == null)
			return null;
		if (o instanceof Integer) {
			return o.toString();
		}
		throw new IllegalArgumentException("IntegerDatatype can only convert "
				+ "values of type java.lang.Integer " + "java.lang.Long");
	}
}
