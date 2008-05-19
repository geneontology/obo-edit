package org.bbop.expression.parser;

import org.bbop.expression.ExpressionException;

import org.apache.log4j.*;

public class BindException extends ExpressionException {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BindException.class);

	public BindException(String msg, int line, int c) {
		super(msg, null, line, c);
	}
	
	public String getMessage() {
		return getLineNumber()+": "+super.getMessage();
	}
}
