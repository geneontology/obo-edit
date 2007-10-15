package org.bbop.dataadapter;

/**
 * The exception thrown by DataAdapter.doOperation(). This exception
 * typically wraps a lower level exception thrown by a data adapter
 * implementation.
 */
public class CancelledAdapterException extends DataAdapterException {

    /**
	 * 
	 */
	private static final long serialVersionUID = 953298067227269355L;

	public CancelledAdapterException() {
	super("Operation cancelled by user");
    }
}
