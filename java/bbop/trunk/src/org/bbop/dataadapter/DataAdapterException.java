package org.bbop.dataadapter;

/**
 * The exception thrown by DataAdapter.doOperation(). This exception typically
 * wraps a lower level exception thrown by a data adapter implementation.
 */
import org.apache.log4j.*;

public class DataAdapterException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DataAdapterException.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = -699185037345949312L;

	public DataAdapterException(String message) {
		super(message);
	}

	public DataAdapterException(Throwable t) {
		super(t);
	}

	public DataAdapterException(String message, Throwable cause) {
		super(message, cause);
	}

	public DataAdapterException(Throwable cause, String message) {
		super(message, cause);
	}
}
