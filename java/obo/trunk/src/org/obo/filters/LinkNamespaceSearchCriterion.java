package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class LinkNamespaceSearchCriterion extends AbstractStringCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkNamespaceSearchCriterion.class);

	public Collection getValues(Collection scratch, Object obj) {
		if (!(obj instanceof Link)) {
//			logger.info("Got bad value in LNSC: " + obj + ", class = "
//					   + obj.getClass());
			return scratch; // Return the empty Collection that was passed in
		}
		if (((Link) obj).getNamespace() != null)
			scratch.add(((Link) obj).getNamespace().getID());
		return scratch;
	}

	public String getID() {
		return "namespace";
	}

	public Class getInputType() {
		return Link.class;
	}

	@Override
	public String toString() {
		return "Namespace";
	}
}
