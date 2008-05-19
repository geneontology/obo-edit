package org.bbop.expression.parser;

import org.apache.log4j.*;

public class ReturnValueException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReturnValueException.class);
	protected Object value;
	
	public ReturnValueException(Object value) {
		this.value = value;
	}
	
	public Object getValue() {
		return value;
	}
}
