package org.bbop.commandline;

import java.util.*;

import org.apache.log4j.*;

public class TagSpec extends OrderedArgumentSignature {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TagSpec.class);

	protected EnumArgumentSignature nameSig = new EnumArgumentSignature();
	protected ArgumentSignature signature;
	protected String primaryName;

	protected TagSpec() {
	}
	
	public TagSpec(String name) {
		this(name, new ValueSpec());
	}

	public TagSpec(String name, ArgumentSignature signature) {
		primaryName = name;
		this.signature = signature;
		nameSig.addSignature(new ValueSpec(primaryName));
		addSignature(nameSig);
		addSignature(signature);
	}

	public String getShortDocumentation() {
		StringBuffer out = new StringBuffer();
		out.append(primaryName);

		String trailingDocs = signature.getShortDocumentation();
		if (trailingDocs.length() > 0)
			out.append(" " + trailingDocs);
		return out.toString();
	}

	public String toString() {
		return "TagSpec('" + primaryName + "'):" + id;
	}

	public void addName(String name) {
		nameSig.addSignature(new ValueSpec(name));
	}

	protected OrderedArgumentSignature createCopyObject() {
		return new TagSpec();
	}

	public ArgumentSignature copy() {
		TagSpec out = (TagSpec) super.copy();
		out.primaryName = primaryName;
		out.signature = signature.copy();
		out.nameSig = (EnumArgumentSignature) nameSig.copy();
		return out;
	}

	public List getValues() throws UnfullfilledException {
		Tag out = new Tag(primaryName);
		Iterator it = signature.getValues().iterator();
		while (it.hasNext()) {
			ArgumentValue av = (ArgumentValue) it.next();
			out.addValue(av);
		}
		return Collections.singletonList(out);
	}
}
