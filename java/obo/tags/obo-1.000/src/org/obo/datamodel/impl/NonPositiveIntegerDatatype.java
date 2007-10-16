package org.obo.datamodel.impl;

import org.obo.datamodel.*;

public class NonPositiveIntegerDatatype extends IntegerDatatype {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2283959268087758315L;

	@Override
	public String getID() {
		return "xsd:nonPositiveInteger";
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public String getComment() {
		return "Represents a non positive integer; an integer <= 0";
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
			if (val > 0)
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
			if (val.intValue() > 0)
				throw new IllegalArgumentException(
						"Non-positive integers must " + "be < 1");
			return val;
		} catch (NumberFormatException ex) {
			throw new IllegalArgumentException(
					"Illegal non-positive integer value " + string);
		}
	}

	@Override
	public String getString(Integer o) {
		if (o == null)
			return null;
		if (o instanceof Integer) {
			int val = ((Number) o).intValue();
			if (val >= 0)
				throw new IllegalArgumentException(
						"Non-positive integers must " + "be < 1");
			return o.toString();
		}
		throw new IllegalArgumentException(
				"NonPositiveIntegerDatatype can only convert "
						+ "values of type java.lang.Integer "
						+ "java.lang.Long");
	}
}
