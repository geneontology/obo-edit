package org.obo.filters;

/*
 * A {@link SearchAspect } that returns the set of all a term's ancestors
 */

import java.util.*;

import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class AncestorSearchAspect implements SearchAspect {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AncestorSearchAspect.class);

	public AncestorSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner,
			Filter traversalFilter, Object o) {
		if (reasoner != null && o instanceof LinkedObject) {
			for (Link link : reasoner.getParents((LinkedObject) o)) {
				if (traversalFilter == null || traversalFilter.satisfies(link)) {
					c.add(link.getParent());
				}
			}
		} else {
			if (o instanceof LinkedObject)
				c.addAll(TermUtil.getAncestors((LinkedObject) o, false));
		}
		return c;
	}
	
	public String getID() {
		return "ancestor";
	}

	@Override
	public boolean equals(Object o) {
		return o.getClass().equals(getClass());
	}

	@Override
	public int hashCode() {
		return getClass().hashCode();
	}

	@Override
	public String toString() {
		return "Ancestor";
	}
}
