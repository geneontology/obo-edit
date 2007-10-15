package org.bbop.commandline;

import java.util.*;

public class Tag implements ArgumentValue {

	protected String name;
	protected List values;

	public Tag(String name) {
		this.name = name;
		values = new ArrayList();
	}

	public List getValues() {
		return values;
	}

	public String getName() {
    	return name;
    }
    
	public String getStringValue() {
		return ((StringValue) values.get(0)).toString();
	}

	public void addValue(ArgumentValue val) {
		values.add(val);
	}

	public String toString() {
		return getName() + ": " + values;
	}
}
