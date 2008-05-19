package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DefinitionDbxrefSearchCriterion extends
	AbstractDbxrefSearchCriterion<DefinedObject> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefinitionDbxrefSearchCriterion.class);

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
