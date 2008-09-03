package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class ParentSearchAspect implements SearchAspect {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ParentSearchAspect.class);

	public ParentSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner,
			Filter traversalFilter, Object o) {
		if (reasoner != null) {
//			for (Link link : reasoner.getParents((LinkedObject) o)) 
//				c.add(link.getParent());
			LinkDatabase trimmedReasoner = new TrimmedLinkDatabase(reasoner);
			for (Link link : trimmedReasoner.getParents((LinkedObject) o)) 
			c.add(link.getParent());

		} else {
			c.addAll(TermUtil.getParents((LinkedObject) o));
		}
		return c;
	}


	public boolean equals(Object o) {
		return o.getClass().equals(getClass());
	}

	@Override
	public int hashCode() {
		return getClass().hashCode();
	}

	public String getID() {
		return "parent";
	}

	public String toString() {
		return "Parent";
	}


}
