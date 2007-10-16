package org.obo.filters;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;

public abstract class AbstractStringCriterion<IN_TYPE> extends AbstractCriterion<IN_TYPE, String> {
	
	public Class<String> getReturnType() {
		return String.class;
	}

	public boolean isLegal(String value) {
		return true;
	}
}
