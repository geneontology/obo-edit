package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DecimalDatatype extends SimpleDatatype<Double> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DecimalDatatype.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 8806419242226244891L;

	@Override
	public String getID() {
		return "xsd:decimal";
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public String getComment() {
		return "Represents a real number";
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
			Double.parseDouble(string);
			return true;
		} catch (NumberFormatException ex) {
			return false;
		}
	}

	@Override
	public Double getValue(String string) {
		if (string == null)
			return null;
		try {
			return new Double(string);
		} catch (NumberFormatException ex) {
			throw new IllegalArgumentException("Illegal decimal value "
					+ string);
		}
	}

	@Override
	public String getString(Double o) {
		if (o == null)
			return null;
		return o.toString();
	}
}
