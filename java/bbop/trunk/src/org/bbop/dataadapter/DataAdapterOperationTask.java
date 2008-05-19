package org.bbop.dataadapter;

import org.bbop.swing.BackgroundEventQueue;
import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.TaskDelegate;

/**
 * A {@link TaskDelegate} that wraps a call to
 * {@link DataAdapter#doOperation(IOOperation, AdapterConfiguration, Object)}. This
 * task can then be scheduled with a {@link BackgroundEventQueue}.
 * 
 * @author jrichter
 * 
 * @param <IN> The input type of the expected io operation
 * @param <OUT> The output type of the expected io operation
 */
import org.apache.log4j.*;

public class DataAdapterOperationTask<IN, OUT> extends
	AbstractTaskDelegate<OUT> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DataAdapterOperationTask.class);

	protected DataAdapter adapter;
	protected IOOperation<IN, OUT> op;
	protected AdapterConfiguration config;
	protected IN input;

	public DataAdapterOperationTask(DataAdapter adapter,
			IOOperation<IN, OUT> op, AdapterConfiguration config, IN input) {
		this.adapter = adapter;
		this.op = op;
		this.input = input;
		this.config = config;
	}

	@Override
	public void cancel() {
		adapter.cancel();
		super.cancel();
	}

	@Override
	public void execute() throws Exception {
		setResults(adapter.doOperation(op, config, input));
	}

	@Override
	public String getProgressString() {
		return adapter.getProgressString();
	}

	@Override
	public Number getProgressValue() {
		return adapter.getProgressValue();
	}
}
