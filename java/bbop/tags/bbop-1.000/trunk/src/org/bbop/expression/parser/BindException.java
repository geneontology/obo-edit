package org.bbop.expression.parser;

import org.bbop.expression.ExpressionException;

public class BindException extends ExpressionException {

	public BindException(String msg, int line, int c) {
		super(msg, null, line, c);
	}
	
	public String getMessage() {
		return getLineNumber()+": "+super.getMessage();
	}
}
