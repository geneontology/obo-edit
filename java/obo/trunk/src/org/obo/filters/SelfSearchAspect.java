package org.obo.filters;

import java.util.*;

import org.obo.reasoner.ReasonedLinkDatabase;


import org.apache.log4j.*;

public class SelfSearchAspect implements SearchAspect {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SelfSearchAspect.class);

	public SelfSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner, Filter traversalFilter, Object o) {
//		logger.debug("SelfSearchAspect.getObjects");
		c.add(o);
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
		return "self";
	}

	@Override
	public String toString() {
		return "Self";
	}
}
