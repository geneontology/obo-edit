package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Dbxref;

public class DbxrefIDSearchCriterion extends AbstractStringCriterion<Dbxref> {

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
