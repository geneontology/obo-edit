package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

public class LinkNamespaceSearchCriterion extends AbstractStringCriterion {

	public Collection getValues(Collection scratch, Object obj) {
		if (!(obj instanceof Link))
			System.err.println("Got bad value in LNSC: " + obj + ", class = "
					+ obj.getClass());
		if (((Link) obj).getNamespace() != null)
			scratch.add(((Link) obj).getNamespace().getID());
		return scratch;
	}

	public String getID() {
		return "namespace";
	}

	public Class getInputType() {
		return Link.class;
	}

	@Override
	public String toString() {
		return "Namespace";
	}
}
