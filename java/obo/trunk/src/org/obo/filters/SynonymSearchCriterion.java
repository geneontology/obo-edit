package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

/* Why does this class extend AbstractCriterion rather than AbstractStringCriterion? */
public class SynonymSearchCriterion extends AbstractCriterion<IdentifiedObject, Synonym> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymSearchCriterion.class);

        protected boolean excludeObsoletes = false;

	public static final SynonymSearchCriterion CRITERION = new SynonymSearchCriterion();
	
	public SynonymSearchCriterion() {
          this(false);
	}	

       public SynonymSearchCriterion(boolean excludeObsoletes) {
//         logger.debug("SynonymSearchCriterion(excludeObsoletes = " + excludeObsoletes);
          this.excludeObsoletes = excludeObsoletes;
       }
	
	public Collection<Synonym> getValues(Collection<Synonym> scratch,
			IdentifiedObject obj) {
		if (obj instanceof SynonymedObject) {
                  if (!(excludeObsoletes && isObsolete((IdentifiedObject) obj))) {
			Iterator it = ((SynonymedObject) obj).getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				scratch.add(s);
			}
                  }
		}
		return scratch;
	}
	
	public String getID() {
		return "synonym";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Synonym";
	}

	public Class<Synonym> getReturnType() {
		return Synonym.class;
	}

        /**
	 * Returns whether the given object is obsolete.
	 */
	public static boolean isObsolete(IdentifiedObject o) {
          return ((o instanceof ObsoletableObject)
                  && ((ObsoletableObject) o).isObsolete());
	}

}
