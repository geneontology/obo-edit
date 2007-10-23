package org.obo.filters;

import java.util.*;

import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ReasonedLinkDatabase;

public interface SearchAspect extends Cloneable {

	public Collection<LinkedObject> getObjects(Collection<LinkedObject> c,
			ReasonedLinkDatabase reasoner, Filter traversalFilter, Object o);
	public String getID();
}
