package org.bbop.commandline;

import java.util.*;

import org.apache.log4j.*;

public class Tag implements ArgumentValue {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Tag.class);

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

	@Override
	public String toString() {
		return getName() + ": " + values;
	}
}
