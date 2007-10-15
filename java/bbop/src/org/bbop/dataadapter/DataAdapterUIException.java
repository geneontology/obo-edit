package org.bbop.dataadapter;

public class DataAdapterUIException extends Exception {

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
