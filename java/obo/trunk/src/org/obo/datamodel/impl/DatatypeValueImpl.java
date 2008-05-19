package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DatatypeValueImpl<T> implements DatatypeValue<T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DatatypeValueImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -4671700958230021853L;
	protected Datatype<T> type;
	protected String value;

	public DatatypeValueImpl(Datatype<T> type, String value) {
		this.type = type;
		this.value = value;
	}

	public Type<T> getType() {
		return type;
	}

	public String getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		return type.getValue(value).hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof DatatypeValue) {
			DatatypeValue<?> dv = (DatatypeValue<?>) o;
			if (!type.equals(dv.getType()))
				return false;
			T thisval = type.getValue(value);
			T oval = ((Datatype<T>) dv.getType()).getValue(dv.getValue());
			return oval.equals(thisval);
		} else
			return false;
	}

	@Override
	public String toString() {
		return "\"" + value + "\" {type=" + type + "}";
	}
}
