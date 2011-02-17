package org.obo.filters;

import java.util.Collection;
import java.util.Iterator;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DbxrefSearchCriterion extends AbstractDbxrefSearchCriterion<IdentifiedObject> {
	protected final static Logger logger = Logger.getLogger(DbxrefSearchCriterion.class);
        protected boolean excludeObsoletes = false;

	public DbxrefSearchCriterion() {
          this(false);
	}	

       public DbxrefSearchCriterion(boolean excludeObsoletes) {
          this.excludeObsoletes = excludeObsoletes;
       }

	@Override
	protected void addDbxrefs(Collection scratch, IdentifiedObject obj) {
          if (excludeObsoletes && (obj instanceof IdentifiedObject) && isObsolete((IdentifiedObject) obj))
            return;

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
				addDbxrefs(scratch, s.getXrefs());
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
