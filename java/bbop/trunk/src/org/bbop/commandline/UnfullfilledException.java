package org.bbop.commandline;

import org.apache.log4j.*;

public class UnfullfilledException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(UnfullfilledException.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = 1941145631189294956L;

	public UnfullfilledException() {
	super();
    }

    public UnfullfilledException(String message) {
	super(message);
    }
}
