package org.obo.filters;

/*
 * Returns the names of all the subsets to which a given
 * {@link org.obo.datamodel.IdentifiedObject }
 * belongs.
 */

import java.util.Collection;
import java.util.Iterator;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class SubsetSearchCriterion extends AbstractStringCriterion {

	protected final static Logger logger = Logger.getLogger(SubsetSearchCriterion.class);

        protected boolean excludeObsoletes = false;

	public SubsetSearchCriterion() {
          this(false);
	}	

       public SubsetSearchCriterion(boolean excludeObsoletes) {
          this.excludeObsoletes = excludeObsoletes;
       }

	public Collection getValues(Collection scratch, Object obj) {
		if (obj instanceof SubsetObject) {
                  if (!(excludeObsoletes && isObsolete((IdentifiedObject) obj))) {
			Iterator it = ((SubsetObject) obj).getSubsets().iterator();
			while (it.hasNext()) {
				TermSubset cat = (TermSubset) it.next();
				scratch.add(cat.getName());
			}
                  }
		}
		return scratch;
	}
	
	public String getID() {
		return "subset";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Subset";
	}
}
