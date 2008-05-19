package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Dbxref;

import org.apache.log4j.*;

public class DbxrefDescSearchCriterion extends AbstractStringCriterion<Dbxref> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefDescSearchCriterion.class);
	public static final DbxrefDescSearchCriterion CRITERION = new DbxrefDescSearchCriterion();

	public String getID() {
		return "dbxref_desc";
	}

	public Class<Dbxref> getInputType() {
		return Dbxref.class;
	}

	public Collection<String> getValues(Collection<String> scratch, Dbxref obj) {
		scratch.add(obj.getDesc());
		return scratch;
	}
}
