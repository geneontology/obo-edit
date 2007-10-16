package org.bbop.dataadapter;

public class DefaultIOOperation<IN, OUT> implements IOOperation<IN, OUT> {

	protected String id;
	protected String name;
	protected Class<IN> inputType;
	protected Class<OUT> outputType;

	public DefaultIOOperation() {
	}

	public DefaultIOOperation(String id, String name, Class<IN> inputType,
			Class<OUT> outputType) {
		this.id = id;
		this.name = name;
		this.inputType = inputType;
		this.outputType = outputType;
	}

	public void setID(String id) {
		this.id = id;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getID() {
		return id;
	}

	public String getName() {
		return name;
	}

	public boolean equals(Object o) {
		if (o instanceof IOOperation) {
			IOOperation op = (IOOperation) o;
			return getID().equals(op.getID());
		} else
			return false;
	}

	public int hashCode() {
		return getID().hashCode();
	}

	public Class<IN> getInputType() {
		return inputType;
	}

	public Class<OUT> getOutputType() {
		return outputType;
	}
}
