package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Dbxref;

public class DbxrefDBSearchCriterion extends AbstractStringCriterion<Dbxref> {
	public static final DbxrefDBSearchCriterion CRITERION = new DbxrefDBSearchCriterion();

	public String getID() {
		return "dbxref_db";
	}

	public Class<Dbxref> getInputType() {
		return Dbxref.class;
	}

	public Collection<String> getValues(Collection<String> scratch, Dbxref obj) {
		scratch.add(obj.getDatabase());
		return scratch;
	}
}
