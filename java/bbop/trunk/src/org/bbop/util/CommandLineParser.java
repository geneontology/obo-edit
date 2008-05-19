package org.bbop.util;

import java.util.*;

import org.apache.log4j.*;

public class CommandLineParser {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CommandLineParser.class);

    public static Tag parse(TagSpec spec, String [] arguments) throws
	CommandLineParseException {
	List argList = new Vector();
	for(int i=0; i < arguments.length; i++)
	    argList.add(arguments[i]);

	return parse(spec, argList, false, false);
    }

    protected static Tag finalizeTag(TagSpec spec, Tag tag,
				     boolean tentative)
	throws CommandLineParseException {
	if (tentative) {
	    logger.info("finalized still-tentative "+tag);
	    return null;
	}

	if (!tag.isImplied()) {
	    Iterator it = spec.getDefaultValues().iterator();
	    while(it.hasNext()) {
		Object defaultObj = it.next();
		Iterator it2 = tag.getArguments().iterator();
		boolean foundDef = false;
		while(it2.hasNext()) {
		    Object o = it2.next();
		    if (o instanceof Tag && defaultObj instanceof Tag) {
			Tag otag = (Tag) o;
			Tag deftag = (Tag) defaultObj;
			if (otag.getName().equals(deftag.getName())) {
			    foundDef = true;
			    break;
			}
		    } else if (o instanceof String &&
			       defaultObj instanceof String) {
			foundDef = true;
			break;
		    }
		}
		if (!foundDef)
		    tag.getArguments().add(0, defaultObj);
	    }
	}

	return tag;
    }

    public static Tag parse(TagSpec spec, List arguments, boolean tentative,
			    boolean takeLiterally)
	throws CommandLineParseException {
	Iterator it;
	Tag tag = new Tag(spec.getName(), tentative);

	Tag orphanTag = null;


	ArgumentSpec impliedSpec = spec.getImpliedSpec();

	ArgumentSpec untypedSpec = new ArgumentSpec(null, 0);
	it = spec.getArgumentSpecs().iterator();
	List argumentSpecs = new Vector();
	while(it.hasNext()) {
	    ArgumentSpec as = (ArgumentSpec) it.next();
	    if (as.getTagSpec() == null)
		untypedSpec = new ArgumentSpec(null, as.getCardinality());
	    else
		argumentSpecs.add(as);
	}

	if (tag.isImplied()) {
	    it = spec.getDefaultValues().iterator();
	    while(it.hasNext()) {
		Object o = it.next();
		tag.getArguments().add(o);

		if (o instanceof String) {
		    if (untypedSpec.getCardinality() > 0)
			untypedSpec = new ArgumentSpec(null,
						       untypedSpec.
						       getCardinality() - 1);
		} else if (o instanceof Tag) {
		    Tag otag = (Tag) o;
		    Iterator it2 = argumentSpecs.iterator();
		    for(int i=0; it2.hasNext(); i++) {
			ArgumentSpec as = (ArgumentSpec) it2.next();
			if (as.getTagSpec().getName().equals(otag.getName())) {
			    if (as.getCardinality() == 1) {
				it2.remove();
			    } else if (as.getCardinality() > 0) {
				argumentSpecs.
				    set(i, new ArgumentSpec(as.getTagSpec(),
							    as.getCardinality() - 1));
			    }
			    break;
			}
		    }
		}
	    }
	}

	it = arguments.iterator();
	while(it.hasNext()) {
	    String s = (String) it.next();
	    it.remove();
	    if (takeLiterally || !s.startsWith("-")) {
		if (untypedSpec.getCardinality() > 0 ||
		    untypedSpec.getCardinality() < 0) {
		    // if possible, add this value as a plain value of this
		    // tag

		    tag.getArguments().add(s);
		    tentative = false;

		    if (untypedSpec.getCardinality() > 0)
			untypedSpec = new ArgumentSpec(null,
						       untypedSpec.
						       getCardinality() - 1);		
		} else if (impliedSpec != null) {
		    // if we can't add any more plain values,
		    // try using the implied spec

		    arguments.add(0, s);
		    Tag childTag = parse(impliedSpec.getTagSpec(),
					 arguments, true, takeLiterally);

		    it = arguments.iterator();

		    if (childTag != null) {
			tag.getArguments().add(childTag);
			tentative = false;
			if (impliedSpec.getCardinality() == 1)
			    impliedSpec = null;
			else if (impliedSpec.getCardinality() > 0) {
			    impliedSpec = new ArgumentSpec(impliedSpec.
							   getTagSpec(),
							   impliedSpec.
							   getCardinality()
							   - 1);
			}
		    } else {
			// if the implied spec couldn't get anything,
			// we're done with this part of the parse
			return finalizeTag(spec, tag, tentative);
		    }
		} else {
		    // if we can't treat this as an implied value parameter,
		    // and we can't treat it as a plain value,
		    // we're done
		    arguments.add(0, s);
		    return finalizeTag(spec, tag, tentative);
		}
		takeLiterally = false;
	    } else if (s.equals("--")) {
		takeLiterally = true;
		continue;
	    } else {
		orphanTag = null;
		// if we recognize the tag, do a recursive call
		// against the tag spec we found
		boolean found = false;
		Iterator it2 = argumentSpecs.iterator();
		for(int i=0; it2.hasNext(); i++) {
		    ArgumentSpec as = (ArgumentSpec) it2.next();
		    if (as.getTagSpec().getName().equals(s)) {
			found = true;

			Tag childTag = parse(as.getTagSpec(),
					     arguments, false, takeLiterally);
			tag.getArguments().add(childTag);
			tentative = false;
			it = arguments.iterator();

			// count down the cardinality on non-infinite
			// items. When an item gets down to zero cardinality
			// remove it from the argumentSpecs list
			if (as.getCardinality() == 1) {
			    it2.remove();
			} else if (as.getCardinality() > 0) {
			    argumentSpecs.
				set(i, new ArgumentSpec(as.getTagSpec(),
						   as.getCardinality() - 1));
			}
			break;
		    }
		}
		if (!found) {
		    arguments.add(0, s);
		    it = arguments.iterator();

		    if (impliedSpec != null) {
			Tag childTag = parse(impliedSpec.getTagSpec(),
					     arguments, true, takeLiterally);
			it = arguments.iterator();
			if (childTag != null) {
			    tentative = false;
			    tag.getArguments().add(childTag);
			    if (impliedSpec.getCardinality() == 1)
				impliedSpec = null;
			    else if (impliedSpec.getCardinality() > 0) {
				impliedSpec = new ArgumentSpec(impliedSpec.
							       getTagSpec(),
							       impliedSpec.
							       getCardinality()
							       - 1);
			    }
			} else {
			    // if the implied spec couldn't get anything,
			    // we're done with this part of the parse
			    return finalizeTag(spec, tag, tentative);
			}
		    } else
			return finalizeTag(spec, tag, tentative);
		}
	    }
	}

	return finalizeTag(spec, tag, tentative);
    }
}
