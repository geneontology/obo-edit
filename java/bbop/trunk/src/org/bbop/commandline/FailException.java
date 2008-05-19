package org.bbop.commandline;

import org.apache.log4j.*;

public class FailException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FailException.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = 5928018651422998765L;

	public FailException(String message) {
	super(message);
    }
}
