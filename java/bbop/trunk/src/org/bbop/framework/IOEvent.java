package org.bbop.framework;

import java.util.EventObject;

import org.bbop.dataadapter.IOOperation;

import org.apache.log4j.*;

public class IOEvent<IN> extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IOEvent.class);
	
	protected IOOperation<IN, ?> op;
	protected IN input;

	public IOEvent(Object source, IOOperation<IN, ?> op, IN obj) {
		super(source);
		this.op = op;
		this.input = obj;
	}
	
	public IOOperation<IN, ?> getOperation() {
		return op;
	}
	
	public IN getInput() {
		return input;
	}
}
