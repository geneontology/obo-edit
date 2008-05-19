package org.bbop.util;

import java.io.Serializable;

import org.apache.log4j.*;

public class NonNegativeInteger extends Number
	implements Comparable, Serializable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NonNegativeInteger.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = -2985436343890130190L;
	public static final int MAX_VALUE = Integer.MAX_VALUE;
    public static final int MIN_VALUE = 0;
    public static final Class TYPE = NonNegativeInteger.class;

    protected int value;

    public NonNegativeInteger(int value) {
	this.value = value;
    }

    public NonNegativeInteger(String value) throws NumberFormatException {
	this(parseInt(value));
    }

    public int compareTo(Object o) {
	return compareTo((Integer) o);
    }

    public int compareTo(Integer integer) {
	if (value < integer.intValue())
	    return -1;
	else if (value > integer.intValue())
	    return 1;
	else
	    return 0;
    }

    public int hashCode() {
	return value;
    }

    public boolean equals(Object o) {
	if (o instanceof NonNegativeInteger)
	    return value == ((NonNegativeInteger) o).intValue();
	else if (o instanceof Integer)
	    return value == ((Integer) o).intValue();
	else
	    return false;
    }

    public int intValue() {
	return value;
    }

    public byte byteValue() {
	return (byte) value;
    }

    public float floatValue() {
	return value;
    }

    public double doubleValue() {
	return value;
    }

    public long longValue() {
	return value;
    }

    public short shortValue() {
	return (short) value;
    }

    public static int parseInt(String s) throws NumberFormatException {
	return parseInt(s, 10);
    }

    public static int parseInt(String s, int radix)
	throws NumberFormatException {
	int out = Integer.parseInt(s.trim(), radix);
	if (out < 0)
	    throw new NumberFormatException("value must be >= 0");
	return out;
    }
}



