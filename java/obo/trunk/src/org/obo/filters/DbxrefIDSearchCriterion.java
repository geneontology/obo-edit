package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Dbxref;

import org.apache.log4j.*;

public class DbxrefIDSearchCriterion extends AbstractStringCriterion<Dbxref> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefIDSearchCriterion.class);

	public static final DbxrefIDSearchCriterion CRITERION =
		new DbxrefIDSearchCriterion();

	public String getID() {
		return "dbxref_id";
	}

	public Class<Dbxref> getInputType() {
		return Dbxref.class;
	}

	public Collection<String> getValues(Collection<String> scratch, Dbxref obj) {
		scratch.add(obj.getDatabaseID());
		return scratch;
	}
}
