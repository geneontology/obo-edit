package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class GeneralDbxrefSearchCriterion extends
	AbstractDbxrefSearchCriterion<DbxrefedObject> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GeneralDbxrefSearchCriterion.class);

	public static final GeneralDbxrefSearchCriterion CRITERION = new GeneralDbxrefSearchCriterion();

	@Override
	protected void addDbxrefs(Collection scratch, DbxrefedObject obj) {
		if (obj instanceof DbxrefedObject) {
			addDbxrefs(scratch, ((DbxrefedObject) obj).getDbxrefs());
		}
	}

	public Class getInputType() {
		return DbxrefedObject.class;
	}

	@Override
	public String getID() {
		return "general_dbxref";
	}

	@Override
	public String toString() {
		return "General dbxref";
	}
}
