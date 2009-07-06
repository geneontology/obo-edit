package org.bbop.util;

import java.util.*;

import org.apache.log4j.*;

public class Tag {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Tag.class);

    protected String name;
    protected List arguments;
    protected boolean implied;

    public Tag(String name, boolean implied) {
	this.implied = implied;
	this.name = name;
	this.arguments = new Vector();
    }

    public boolean isImplied() {
	return implied;
    }

    public String getName() {
	return name;
    }

    public List getArguments() {
	return arguments;
    }

    @Override
	public String toString() {
	return name+": "+arguments;
    }
}
