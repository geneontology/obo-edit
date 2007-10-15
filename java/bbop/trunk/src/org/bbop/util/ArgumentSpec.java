package org.bbop.util;

public class ArgumentSpec {

    protected int cardinality;
    protected TagSpec tagSpec;
    protected String documentation;

    public ArgumentSpec(TagSpec tagSpec,
			int cardinality) {
	this(tagSpec, cardinality, null);
    }

    public ArgumentSpec(TagSpec tagSpec,
			int cardinality,
			String documentation) {
	this.tagSpec = tagSpec;
	this.cardinality = cardinality;
	if (documentation == null) {
	    if (tagSpec != null)
		this.documentation = tagSpec.getDocumentation();
	    else if (cardinality == -1) {
		this.documentation = "arg1 arg2 ...";
	    } else {
		for(int i=0; i < cardinality; i++)
		    this.documentation += (i > 0 ? " " : "") + "arg"+(i+1);
	    }
	}
    }

    public String getDocumenation() {
	if (tagSpec == null) {
	    return "none";
	} else
	    return tagSpec.getDocumentation();
    }

    public String getSummaryDocumentation() {
	return documentation;
    }

    public TagSpec getTagSpec() {
	return tagSpec;
    }

    public int getCardinality() {
	return cardinality;
    }
}
