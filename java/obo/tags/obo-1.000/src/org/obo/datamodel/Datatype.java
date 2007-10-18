package org.obo.datamodel;

import org.obo.datamodel.impl.*;

public interface Datatype<T> extends Type<T> {

	public static final SimpleDatatype<?> SIMPLE_TYPE = new SimpleDatatype<Object>();
	public static final StringDatatype STRING = new StringDatatype();
	public static final IntegerDatatype INTEGER = new IntegerDatatype();
	public static final DecimalDatatype DECIMAL = new DecimalDatatype();
	public static final NegativeIntegerDatatype NEGATIVE_INTEGER = new NegativeIntegerDatatype();
	public static final PositiveIntegerDatatype POSITIVE_INTEGER = new PositiveIntegerDatatype();
	public static final NonNegativeIntegerDatatype NON_NEGATIVE_INTEGER = new NonNegativeIntegerDatatype();
	public static final NonPositiveIntegerDatatype NON_POSITIVE_INTEGER = new NonPositiveIntegerDatatype();
	public static final BooleanDatatype BOOLEAN = new BooleanDatatype();
	public static final Datatype DURATION = null;
	public static final DateDatatype DATE = new DateDatatype();

	public static final Datatype<?>[] DATATYPES = { SIMPLE_TYPE, STRING, INTEGER,
			DECIMAL, NEGATIVE_INTEGER, POSITIVE_INTEGER, NON_NEGATIVE_INTEGER,
			NON_POSITIVE_INTEGER, BOOLEAN, DATE };

	public boolean isAbstract();

	public Datatype<? super T> getSupertype();

	public boolean isLegalValue(String string);

	public T getValue(String string);

	public String getString(T o);
}
