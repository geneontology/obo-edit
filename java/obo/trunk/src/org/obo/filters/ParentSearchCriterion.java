package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class ParentSearchCriterion extends
	AbstractCriterion<IdentifiedObject, Link> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ParentSearchCriterion.class);

	public static final ParentSearchCriterion CRITERION = new ParentSearchCriterion();

	public ParentSearchCriterion() {
	}

	public String getID() {
		return "parent";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "ParentSearchCriterion";
	}

	public Class<Link> getReturnType() {
		return Link.class;
	}

	public Collection<Link> getValues(Collection<Link> scratch,
			IdentifiedObject obj) {
		if (obj instanceof LinkedObject) {
			if (reasoner == null) {
				for (Link link : ((LinkedObject) obj).getParents())
					// ! Why refuse to include intersection links?  
					// This clause prevents some link searches from working.
//					if (!TermUtil.isIntersection(link))
						scratch.add(link);
			} else {
				for (Link link : trimmedReasoner
						.getParents(((LinkedObject) obj))) {
					// ! Why refuse to include intersection links?
//					if (!TermUtil.isIntersection(link))
						scratch.add(link);
				}
			}
		}
		return scratch;
	}
}
