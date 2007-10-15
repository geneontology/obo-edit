package org.bbop.dataadapter;

/**
 * The exception thrown by DataAdapter.doOperation(). This exception typically
 * wraps a lower level exception thrown by a data adapter implementation.
 */
public class DataAdapterException extends Exception {

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
