package org.obo.filters;

import java.util.*;

import org.obo.reasoner.ReasonedLinkDatabase;


public class SelfSearchAspect implements SearchAspect {

	public SelfSearchAspect() {
	}

	public Collection getObjects(Collection c, ReasonedLinkDatabase reasoner, Filter traversalFilter, Object o) {
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
