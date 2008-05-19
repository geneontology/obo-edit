package org.bbop.util;


import org.apache.log4j.*;

public class ExceptionLogger {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExceptionLogger.class);
	public void handle(Throwable t) {
		Logger global = Logger.getLogger("");
		global.log(Level.FATAL, "Uncaught event dispatch exception", t);
	}
}
