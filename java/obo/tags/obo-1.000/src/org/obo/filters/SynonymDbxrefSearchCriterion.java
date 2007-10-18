package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public class SynonymDbxrefSearchCriterion extends
		AbstractDbxrefSearchCriterion<Synonym> {

	public static final SynonymDbxrefSearchCriterion CRITERION = new SynonymDbxrefSearchCriterion();

	@Override
	protected void addDbxrefs(Collection scratch, Synonym s) {
		addDbxrefs(scratch, s.getDbxrefs());
	}

	public Class<Synonym> getInputType() {
		return Synonym.class;
	}

	@Override
	public String getID() {
		return "synonym_dbxref";
	}

	@Override
	public String toString() {
		return "Synonym dbxref";
	}
}
