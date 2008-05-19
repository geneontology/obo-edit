package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class NegativeIntegerDatatype extends IntegerDatatype {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NegativeIntegerDatatype.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 6094346806278269220L;

	@Override
	public String getID() {
		return "xsd:negativeInteger";
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public String getComment() {
		return "Represents a negative integer";
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
		return Datatype.INTEGER;
	}

	@Override
	public boolean isLegalValue(String string) {
		if (string == null)
			return false;
		try {
			int val = Integer.parseInt(string);
			if (val >= 0)
				return false;
			else
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
			Integer val = new Integer(string);
			if (val.intValue() >= 0)
				throw new IllegalArgumentException("Negative integers must "
						+ "be < 0");
			return val;
		} catch (NumberFormatException ex) {
			throw new IllegalArgumentException(
					"Illegal negative integer value " + string);
		}
	}

	@Override
	public String getString(Integer o) {
		if (o == null)
			return null;
		if (o instanceof Integer) {
			int val = ((Number) o).intValue();
			if (val >= 0)
				throw new IllegalArgumentException("Negative integers must "
						+ "be < 0");
			return o.toString();
		}
		throw new IllegalArgumentException(
				"NegativeIntegerDatatype can only convert "
						+ "values of type java.lang.Integer "
						+ "java.lang.Long");
	}
}
