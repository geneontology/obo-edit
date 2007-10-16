package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public class DefinitionDbxrefSearchCriterion extends
		AbstractDbxrefSearchCriterion<DefinedObject> {

	public static final DefinitionDbxrefSearchCriterion CRITERION
	= new DefinitionDbxrefSearchCriterion();

	@Override
	protected void addDbxrefs(Collection scratch, DefinedObject obj) {
		if (obj instanceof DefinedObject) {
			addDbxrefs(scratch, obj.getDefDbxrefs());
		}
	}
	
	public Class<DefinedObject> getInputType() {
		return DefinedObject.class;
	}

	@Override
	public String getID() {
		return "definition_dbxref";
	}

	@Override
	public String toString() {
		return "Definition dbxref";
	}
}
