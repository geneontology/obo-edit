package org.bbop.dataadapter;

public interface IOOperation<IN, OUT> {

	public static class ReadOperation<IN, OUT> extends
			DefaultIOOperation<IN, OUT> {
		public ReadOperation(Class<IN> inputType, Class<OUT> outputType) {
			super("READ", "read", inputType, outputType);
		}
	}

	public static class WriteOperation<IN, OUT> extends
			DefaultIOOperation<IN, OUT> {
		public WriteOperation(Class<IN> inputType, Class<OUT> outputType) {
			super("WRITE", "write", inputType, outputType);
		}
	}

	/*
	public static final IOOperation<Object, Object> READ = new ReadOperation<Object, Object>(Object.class, Object.class);

	public static final IOOperation<Object, Object> WRITE = new WriteOperation<Object, Object>(Object.class, Object.class);
*/
	public String getID();

	public String getName();

	public Class<IN> getInputType();

	public Class<OUT> getOutputType();
}
