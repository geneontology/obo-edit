package org.bbop.commandline;

public class StringValue implements ArgumentValue {
    protected String value;

    public StringValue(String value) {
	this.value = value;
    }

    public String toString() {
	return value;
    }
}
