package org.bbop.dataadapter;

/**
 * A basic interface for data input and output. This interface specifies
 * nothing about the storage/access method, the kind of operation,
 * or what sort of data will be manipulated. This class specifies a standard
 * data adapter interface to allow widgets for data adapter manipulation
 * to be built.
 */

import org.bbop.util.ProgressValued;

public interface DataAdapter extends ProgressValued {

	/**
	 * Returns a unique ID for this data adapter.
	 */
	public String getID();

	/**
	 * Returns a descriptive name for this data adapter
	 */
	public String getName();

	/**
	 * Returns the preferred ui for this data adapter. This method may return
	 * null.
	 */
	public DataAdapterUI getPreferredUI();

	/**
	 * Returns the current data adapter configuration. If doOperation() has been
	 * successfully called, this method will turn whatever configuration was
	 * last used. Otherwise, this method will return null, or a default
	 * configuration.
	 */
	public AdapterConfiguration getConfiguration();

	/**
	 * Returns a list of IOOperations that this adapter supports.
	 */
	public IOOperation[] getSupportedOperations();

	/**
	 * Performs the specified operation on the input using the given
	 * configuration. The input value may be null if executing a read-only
	 * operation. The return value may be null if executing a write-only
	 * operation.
	 */
	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			AdapterConfiguration configuration, INPUT_TYPE input)
			throws DataAdapterException;

	/**
	 * Cancels an operation in progress. This method should only be called
	 * during a threaded doOperation() call.
	 */
	public void cancel();

}
