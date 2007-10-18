package org.oboedit.verify;

public class CheckConfiguration {

	protected byte condition;

	public CheckConfiguration() {
	}

	public byte getCondition() {
		return condition;
	}

	public void setCondition(byte condition) {
		this.condition = condition;
	}

	@Override
	public String toString() {
		return "condition=" + condition;
	}
}
