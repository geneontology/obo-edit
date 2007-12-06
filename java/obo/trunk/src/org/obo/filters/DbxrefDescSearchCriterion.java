package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Dbxref;

public class DbxrefDescSearchCriterion extends AbstractStringCriterion<Dbxref> {
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
