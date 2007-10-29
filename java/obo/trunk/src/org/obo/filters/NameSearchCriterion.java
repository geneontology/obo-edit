package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

public class NameSearchCriterion extends
		AbstractStringCriterion<IdentifiedObject> {

	public static final NameSearchCriterion CRITERION = new NameSearchCriterion();

	public NameSearchCriterion() {
		// TODO Auto-generated constructor stub
	}
	
	public Collection<String> getValues(Collection<String> scratch,
			IdentifiedObject obj) {
		scratch.add(obj.getName());
		return scratch;
	}

	public int getMaxCardinality() {
		return 1;
	}
	
	public int getMinCardinality() {
		return 1;
	}


	public String getID() {
		return "name";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Name";
	}
}
