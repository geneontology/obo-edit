package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;

import org.apache.log4j.*;

public class TermParentSearchCriterion extends AbstractCriterion<IdentifiedObject, Link> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermParentSearchCriterion.class);

	public String getID() {
		return "parent_links";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	public Class<Link> getReturnType() {
		return Link.class;
	}

	public Collection<Link> getValues(Collection<Link> scratch,
			IdentifiedObject obj) {
		if (obj instanceof LinkedObject) {
			for(Link link : ((LinkedObject) obj).getParents()) {
				scratch.add(link);
			}
		}
		return scratch;
	}


}
