package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

public class NamespaceSearchCriterion extends AbstractStringCriterion {

	public Collection getValues(Collection scratch, Object obj) {
		Namespace ns = ((IdentifiedObject) obj).getNamespace();
		if (ns != null)
			scratch.add(ns.getID());
		return scratch;
	}

	public String getID() {
		return "namespace";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Namespace";
	}
}
