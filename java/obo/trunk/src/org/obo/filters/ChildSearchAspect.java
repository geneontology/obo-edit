package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class ChildSearchAspect implements SearchAspect {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ChildSearchAspect.class);

	public ChildSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner,
			Filter traversalFilter, Object o) {
		if (reasoner != null) {
//			for (Link link : reasoner.getChildren((LinkedObject) o)) 
//				c.add(link.getChild());
			LinkDatabase trimmedReasoner = new TrimmedLinkDatabase(reasoner);
			for (Link link : trimmedReasoner.getChildren((LinkedObject) o)) 
			c.add(link.getChild());

		} else {
			c.addAll(TermUtil.getChildren((LinkedObject) o));
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
		return "child";
	}

	public String toString() {
		return "Children";
	}


}


