package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class NameSearchCriterion extends
	AbstractStringCriterion<IdentifiedObject> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NameSearchCriterion.class);

        protected boolean excludeObsoletes = false;
        // We end up making two of these criteria, and they need different IDs.
        protected String id = "name";

	public static final NameSearchCriterion CRITERION = new NameSearchCriterion();

	public NameSearchCriterion() {
          this(false);
	}

        public NameSearchCriterion(boolean excludeObsoletes) {
         this.excludeObsoletes = excludeObsoletes;
       }

        public NameSearchCriterion(boolean excludeObsoletes, String ID) {
//         logger.debug("NameSearchCriterion(excludeObsoletes = " + excludeObsoletes + ", id = " + ID + ")");
         this.excludeObsoletes = excludeObsoletes;
         this.id = ID;
       }
	
	public Collection<String> getValues(Collection<String> scratch,
			IdentifiedObject obj) {
          if ((obj.getName() != null)
              && !(excludeObsoletes && isObsolete(obj)))
              scratch.add(obj.getName());
//          else { // DEL
//            logger.debug("NameSearchCriterion.excludeObsoletes = " + excludeObsoletes + "--Excluding obsolete object (name = " + obj.getName() + ", id = " + obj.getID() + ") from NameSearchCriterion.getValues"); // DEL
//          }
		return scratch;
	}

	public int getMaxCardinality() {
		return 1;
	}
	
	public int getMinCardinality() {
		return 1;
	}

	public String getID() {
          return id;
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Name";
	}
}
