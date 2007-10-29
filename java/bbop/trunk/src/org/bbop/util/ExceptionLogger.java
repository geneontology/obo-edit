package org.bbop.util;

import java.util.logging.Level;
import java.util.logging.Logger;

public class ExceptionLogger {
	public void handle(Throwable t) {
		Logger global = Logger.getLogger("");
		global.log(Level.SEVERE, "Uncaught event dispatch exception", t);
	}
}
