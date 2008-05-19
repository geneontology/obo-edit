package org.bbop.commandline;

import org.apache.log4j.*;

public class StringValue implements ArgumentValue {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(StringValue.class);
    protected String value;

    public StringValue(String value) {
	this.value = value;
    }

    public String toString() {
	return value;
    }
}
