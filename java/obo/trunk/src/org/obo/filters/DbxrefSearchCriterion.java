package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DbxrefSearchCriterion extends AbstractDbxrefSearchCriterion<IdentifiedObject> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DbxrefSearchCriterion.class);

	@Override
	protected void addDbxrefs(Collection scratch, IdentifiedObject obj) {
		if (obj instanceof DbxrefedObject) {
			addDbxrefs(scratch, ((DbxrefedObject) obj).getDbxrefs());
		}
		if (obj instanceof DefinedObject) {
			addDbxrefs(scratch, ((DefinedObject) obj).getDefDbxrefs());
		}
		if (obj instanceof SynonymedObject) {
			Iterator it = ((SynonymedObject) obj).getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				addDbxrefs(scratch, s.getDbxrefs());
			}
		}
	}

	@Override
	public String getID() {
		return "dbxref";
	}

	@Override
	public String toString() {
		return "Dbxref";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}
}
