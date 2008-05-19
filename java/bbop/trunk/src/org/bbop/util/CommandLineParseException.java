package org.bbop.util;

import org.apache.log4j.*;

public class CommandLineParseException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CommandLineParseException.class);
    /**
	 * 
	 */
	private static final long serialVersionUID = 3800944208477893722L;

	public CommandLineParseException(String message) {
	super(message);
    }
}
