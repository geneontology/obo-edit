package org.bbop.expression.parser;

import org.apache.log4j.*;

public class BreakLoopException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BreakLoopException.class);
	public BreakLoopException() {
	}

}
