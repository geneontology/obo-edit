package org.bbop.dataadapter;

import org.bbop.util.AbstractTaskDelegate;

public class DataAdapterOperationTask<IN, OUT> extends AbstractTaskDelegate<OUT> {

	protected DataAdapter adapter;
	protected IOOperation<IN, OUT> op;
	protected AdapterConfiguration config;
	protected IN input;

	public DataAdapterOperationTask(
			DataAdapter adapter,
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
