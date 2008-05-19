package org.obo.datamodel.impl;

import org.obo.datamodel.*;

import java.util.Date;
import java.text.SimpleDateFormat;
import java.text.ParseException;

import org.apache.log4j.*;

public class DateDatatype extends SimpleDatatype<Date> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DateDatatype.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -8087786198895424706L;
	protected SimpleDateFormat simpleDateFormat = new SimpleDateFormat();

	@Override
	public String getID() {
		return "xsd:date";
	}

	@Override
	public String getName() {
		return getID();
	}

	@Override
	public String getComment() {
		return "Represents a date";
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
		try {
			parse(string);
			return true;
		} catch (ParseException ex) {
			return false;
		}
	}

	protected Date parse(String string) throws ParseException {
		return simpleDateFormat.parse(string);
	}

	@Override
	public Date getValue(String string) {
		try {
			return parse(string);
		} catch (ParseException ex) {
			throw new IllegalArgumentException("Cannot convert " + string
					+ "to a date");
		}
	}

	@Override
	public String getString(Date o) {
		return simpleDateFormat.format(o);
	}
}
