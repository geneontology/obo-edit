package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class IsCompleteLinkCriterion extends AbstractBooleanLinkCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsCompleteLinkCriterion.class);

	public boolean matches(Link o) {
		return TermUtil.isIntersection((Link) o);
	}

	@Override
	public Class<Link> getInputType() {
		return Link.class;
	}

	public String getID() {
		return "is_intersection_link";
	}

	@Override
	public String toString() {
		return "Is intersection";
	}
}
