package org.bbop.dataadapter;

/**
 * The exception thrown by DataAdapter.doOperation(). This exception
 * typically wraps a lower level exception thrown by a data adapter
 * implementation.
 */
import org.apache.log4j.*;

public class CancelledAdapterException extends DataAdapterException {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CancelledAdapterException.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = 953298067227269355L;

	public CancelledAdapterException() {
	super("Operation cancelled by user");
    }
}
