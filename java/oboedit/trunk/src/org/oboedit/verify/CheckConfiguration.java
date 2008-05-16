package org.oboedit.verify;

import org.apache.log4j.*;

public class CheckConfiguration {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CheckConfiguration.class);

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
