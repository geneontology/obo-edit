package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class DescendantSearchAspect implements SearchAspect {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DescendantSearchAspect.class);

	public DescendantSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner,
			Filter traversalFilter, Object o) {
		logger.debug("DescendantSearchAspect.getObjects");
		if (reasoner != null && o instanceof LinkedObject) {
//			Collection<Link> reasonedChildren = reasoner.getChildren((LinkedObject) o);		
//			logger.debug("DescendantreasonedChildren.size(): " + reasonedChildren.size());
			for (Link link : reasoner.getChildren((LinkedObject) o)) {
				if (traversalFilter == null || traversalFilter.satisfies(link)) {
					c.add(link.getChild());
				}
			} 
		}
		else {
			if (o instanceof LinkedObject)
				c.addAll(TermUtil.getDescendants((LinkedObject) o, false));
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
		return "descendant";
	}

	@Override
	public String toString() {
		return "Descendant";
	}
}
