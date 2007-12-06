package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

public class ParentSearchCriterion extends
		AbstractCriterion<IdentifiedObject, Link> {

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
		return "parent";
	}

	public Class<Link> getReturnType() {
		return Link.class;
	}

	public Collection<Link> getValues(Collection<Link> scratch,
			IdentifiedObject obj) {
		if (obj instanceof LinkedObject) {
			if (reasoner == null) {
				for (Link link : ((LinkedObject) obj).getParents())
					if (!TermUtil.isIntersection(link))
						scratch.add(link);
			} else {
				for (Link link : trimmedReasoner
						.getParents(((LinkedObject) obj))) {
					if (!TermUtil.isIntersection(link))
						scratch.add(link);
				}
			}
		}
		return scratch;
	}
}
