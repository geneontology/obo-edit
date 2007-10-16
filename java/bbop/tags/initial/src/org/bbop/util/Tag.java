package org.bbop.util;

import java.util.*;

public class Tag {

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

    public String toString() {
	return name+": "+arguments;
    }
}
