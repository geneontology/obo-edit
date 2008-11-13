package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class SynonymDbxrefSearchCriterion extends
	AbstractDbxrefSearchCriterion<Synonym> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymDbxrefSearchCriterion.class);

	public static final SynonymDbxrefSearchCriterion CRITERION = new SynonymDbxrefSearchCriterion();

	@Override
	protected void addDbxrefs(Collection scratch, Synonym s) {
		addDbxrefs(scratch, s.getXrefs());
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
