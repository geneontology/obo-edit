package org.obo.postcomp;

import org.obo.datamodel.OBOProperty;

/**
 * The default TypeNameProvider implementation. Normally, this will just return
 * the result of the {@link OBOProperty#getName()} method.
 * 
 * The one exception is if the OBOProperty id is "part_of". For some reason,
 * almost every ontology designer prefers to name the "part_of" relationship
 * type "part of" rather than "is part of". This leads to strange grammatical
 * problems when auto-generating term names, so the DefaultTypeNamespace handles
 * these automatically.
 * 
 * To turn off the special handling of part of, instantiate the class using the
 * {@link #DefaultTypeNameProvider(boolean)} constructor.
 * 
 * @author jrichter
 * 
 */
import org.apache.log4j.*;

public class DefaultTypeNameProvider implements TypeNameProvider {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultTypeNameProvider.class);

	protected boolean fixPartOf = true;

	/**
	 * Instantiates this class with part_of name fixing enabled
	 */
	public DefaultTypeNameProvider() {

	}

	/**
	 * Instantiates this class with the option to disable part_of name fixing.
	 * 
	 * @param fixPartOf
	 *            Whether or not to automatically change the name of "part_of"
	 *            relationship types to "is part of"
	 */
	public DefaultTypeNameProvider(boolean fixPartOf) {
		this.fixPartOf = fixPartOf;
	}

	public String getName(OBOProperty p) {
		if (fixPartOf && p.getID().equals("part_of"))
			return "is part of";
		else
			return p.getName();
	}

}
