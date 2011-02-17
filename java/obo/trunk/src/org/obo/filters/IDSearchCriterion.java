package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class IDSearchCriterion extends AbstractStringCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDSearchCriterion.class);

        protected boolean excludeObsoletes = false;
        // We end up making two of these criteria, and they need different IDs.
        protected String id = "id";

        public IDSearchCriterion() {
          this(false);
        }

       public IDSearchCriterion(boolean excludeObsoletes) {
//         logger.debug("IDSearchCriterion(excludeObsoletes = " + excludeObsoletes);
          this.excludeObsoletes = excludeObsoletes;
       }

       public IDSearchCriterion(boolean excludeObsoletes, String ID) {
//         logger.debug("IDSearchCriterion(excludeObsoletes = " + excludeObsoletes);
          this.excludeObsoletes = excludeObsoletes;
          this.id = ID;
       }

	public Collection getValues(Collection scratch, Object obj) {
                if (excludeObsoletes && isObsolete((IdentifiedObject) obj)) {
//                  logger.debug("IDSearchCriterion: excludeObsoletes = " + excludeObsoletes + "; excluding " + obj + " because it is obsolete"); // DEL
                  return scratch;
                }

		scratch.add(((IdentifiedObject) obj).getID());
		if (obj instanceof MultiIDObject) {
			Iterator it = ((MultiIDObject) obj).getSecondaryIDs().iterator();
			while (it.hasNext()) {
				scratch.add(it.next());
			}
		}
		return scratch;
	}

	public String getID() {
		return id;
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "ID";
	}
}
