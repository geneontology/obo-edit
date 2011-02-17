package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class GeneralDbxrefSearchCriterion extends
	AbstractDbxrefSearchCriterion<DbxrefedObject> {

	protected final static Logger logger = Logger.getLogger(GeneralDbxrefSearchCriterion.class);

	public static final GeneralDbxrefSearchCriterion CRITERION = new GeneralDbxrefSearchCriterion();

        protected boolean excludeObsoletes = false;

	public GeneralDbxrefSearchCriterion() {
          this(false);
	}	

       public GeneralDbxrefSearchCriterion(boolean excludeObsoletes) {
          this.excludeObsoletes = excludeObsoletes;
       }

	@Override
	protected void addDbxrefs(Collection scratch, DbxrefedObject obj) {
		if (obj instanceof DbxrefedObject) {
                  if (excludeObsoletes && (obj instanceof IdentifiedObject) && isObsolete((IdentifiedObject) obj))
                    return;
                  else
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
