package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DefinitionDbxrefSearchCriterion extends
	AbstractDbxrefSearchCriterion<DefinedObject> {

	protected final static Logger logger = Logger.getLogger(DefinitionDbxrefSearchCriterion.class);

	public static final DefinitionDbxrefSearchCriterion CRITERION
	= new DefinitionDbxrefSearchCriterion();

        protected boolean excludeObsoletes = false;

	public DefinitionDbxrefSearchCriterion() {
          this(false);
	}	

       public DefinitionDbxrefSearchCriterion(boolean excludeObsoletes) {
          this.excludeObsoletes = excludeObsoletes;
       }

	@Override
	protected void addDbxrefs(Collection scratch, DefinedObject obj) {
		if (obj instanceof DefinedObject) {
                  if (!(excludeObsoletes && (obj instanceof IdentifiedObject) && isObsolete((IdentifiedObject) obj)))
			addDbxrefs(scratch, obj.getDefDbxrefs());
		}
	}
	
	public Class<DefinedObject> getInputType() {
		return DefinedObject.class;
	}

	@Override
	public String getID() {
		return "definition_dbxref";
	}

	@Override
	public String toString() {
		return "Definition dbxref";
	}
}
