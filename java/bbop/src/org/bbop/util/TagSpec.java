package org.bbop.util;

import java.util.*;

public class TagSpec implements Cloneable {

    protected ArgumentSpec impliedSpec;
    protected String name;
    protected List argumentSpecs;
    protected List impliedArgs;
    protected List defaultValues;
    protected String documentation;

    public TagSpec() {
	this("");
    }

    public TagSpec(String name) {
	this(name, null);
    }

    public TagSpec(String name, String documentation) {
	this.name = name;
	this.documentation = documentation;
	this.argumentSpecs = new Vector();
	this.impliedArgs = new Vector();
	this.defaultValues = new Vector();
    }

    public TagSpec copy(String name) {
	TagSpec out = (TagSpec) clone();
	out.name = name;
	return out;
    }

    public Object clone() {
	try {
	    return super.clone();
	} catch (Exception ex) {
	    return this;
	}
    }

    public String getName() {
	return name;
    }

    public List getDefaultValues() {
	return defaultValues;
    }

    public void setImpliedSpec(TagSpec spec, int cardinality) {
	setImpliedSpec(new ArgumentSpec(spec, cardinality));
    }

    public void setImpliedSpec(ArgumentSpec impliedSpec) {
	this.impliedSpec = impliedSpec;
    }

    public ArgumentSpec getImpliedSpec() {
	return impliedSpec;
    }

    public void addArgumentSpec(TagSpec spec, int cardinality) {
	addArgumentSpec(new ArgumentSpec(spec, cardinality));
    }

    public void addArgumentSpec(TagSpec spec, int cardinality,
				String documentation) {
	addArgumentSpec(new ArgumentSpec(spec, cardinality, documentation));
    }

    public void addArgumentSpec(ArgumentSpec spec) {
	argumentSpecs.add(spec);
    }

    public void addImpliedArgument(Tag tag) {
	impliedArgs.add(tag);
    }

    public List getImpliedArguments() {
	return impliedArgs;
    }

    public List getArgumentSpecs() {
	return argumentSpecs;
    }

    public String getDocumentation() {
	return getSummaryDocumentation();
    }

    public String getSummaryDocumentation() {
	if (documentation != null)
	    return name+" "+documentation;
	else {
	    StringBuffer out = new StringBuffer();
	    out.append(name);
	    out.append(" {");
	    Iterator it = argumentSpecs.iterator();
	    while(it.hasNext()) {
		ArgumentSpec as = (ArgumentSpec) it.next();
		out.append(" ");
		out.append(as.getSummaryDocumentation());
	    }
	    out.append("}");
	
	    return out.toString();
	}
    }

    public String toString() {
	return getName();
    }
}
