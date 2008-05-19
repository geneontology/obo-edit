package org.bbop.dataadapter;

/**
 * An exception thrown by a DataAdapterUI. This kind of exception is thrown when
 * the UI detects some kind of problem before an {@link AdapterConfiguration} is
 * passed to a {@link DataAdapter}
 * @author jrichter
 *
 */
import org.apache.log4j.*;

public class DataAdapterUIException extends Exception {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DataAdapterUIException.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = -4304594851035509695L;
	protected boolean showMessage;

    public DataAdapterUIException() {
	this(false);
    }

    public DataAdapterUIException(boolean showMessage) {
	super();
	this.showMessage = showMessage;
    }

    public DataAdapterUIException(String message) {
	this(message, true);
    }

    public DataAdapterUIException(String message, boolean showMessage) {
	super(message);
	this.showMessage = showMessage;
    }

    public boolean showMessage() {
	return showMessage;
    }
}
