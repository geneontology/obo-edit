package org.obo.filters;

/*
 * A {@link SearchAspect } that returns a term's root.
 */

import java.util.*;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

public class RootSearchAspect implements SearchAspect {

	public RootSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner,
			Filter traversalFilter, Object o) {
		if (reasoner != null) {
			for (Link link : reasoner.getParents((LinkedObject) o)) {
				if ((traversalFilter == null || traversalFilter.satisfies(link))
						&& reasoner.getParents((LinkedObject) o).size() == 0) {
					c.add(link.getParent());
				}
			}
		} else {
			LinkedObject root = TermUtil.getRoot((LinkedObject) o);
			if (o instanceof LinkedObject)
				c.add(root);
			else if (o instanceof Link)
				c.add(new OBORestrictionImpl(root));
		}
		return c;
	}

	@Override
	public boolean equals(Object o) {
		return o.getClass().equals(getClass());
	}

	@Override
	public int hashCode() {
		return getClass().hashCode();
	}
	
	public String getID() {
		return "root";
	}

	@Override
	public String toString() {
		return "Root";
	}
}
