package org.bbop.expression.parser;

public class ReturnValueException extends Exception {
	protected Object value;
	
	public ReturnValueException(Object value) {
		this.value = value;
	}
	
	public Object getValue() {
		return value;
	}
}
