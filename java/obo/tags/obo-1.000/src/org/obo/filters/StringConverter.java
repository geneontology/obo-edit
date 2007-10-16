package org.obo.filters;

public interface StringConverter<T> {
	public static final StringConverter DEFAULT = new DefaultStringConverter();
	
	public String convert(T o);
}
