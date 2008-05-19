package org.bbop.expression;

import org.bbop.expression.parser.SimpleNode;

import org.apache.log4j.*;

public class ExpressionException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExpressionException.class);
	protected int lineNumber = -1;
	protected int charNumber = -1;

	public ExpressionException() {
		super();
	}
	
	public ExpressionException(String message) {
		this(message, null, -1, -1);
	}
	
	public ExpressionException(Exception parent) {
		this(parent, -1, -1);
	}
	
	public ExpressionException(Exception parent, int lineNumber, int charNumber) {
		this(parent.getMessage(), parent, lineNumber, charNumber);
	}

	public ExpressionException(String message, Exception parent, int lineNumber, int charNumber) {
		super(message, parent);
		this.lineNumber = lineNumber;
		this.charNumber  = charNumber;
	}
	
	public void decorateException(SimpleNode node) throws ExpressionException {
		this.lineNumber = node.getLineNum();
		this.charNumber = node.getCharNum();
		throw this;
	}
	
	public void setLineNumber(int lineNumber) {
		this.lineNumber = lineNumber;
	}
	
	public int getLineNumber() {
		return lineNumber;
	}

	public int getCharNumber() {
		return charNumber;
	}

	public void setCharNumber(int charNumber) {
		this.charNumber = charNumber;
	}
}
