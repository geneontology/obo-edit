package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

/**
 * IsIntersectionCriterion - used to filter out terms with xps
 * Currently being used by the GlobalTermRenderer Interface
 * */
public class IsIntersectionCriterion extends AbstractBooleanCriterion{

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsIntersectionCriterion.class);

	public String getID() {
		return "is_intersection";
	}

	public boolean matches(IdentifiedObject o) {
		LinkedObject lo = (LinkedObject) o;
		return TermUtil.isIntersection(lo);
	}

	@Override
	public String toString() {
		return "Is Intersection";
	}

}
