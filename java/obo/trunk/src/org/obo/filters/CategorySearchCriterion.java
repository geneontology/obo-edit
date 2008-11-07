package org.obo.filters;

/*
 * Returns the names of all the categories to which a given
 * {@link org.obo.datamodel.IdentifiedObject }
 * belongs.
 *
 */

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class CategorySearchCriterion extends AbstractStringCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CategorySearchCriterion.class);

	public Collection getValues(Collection scratch, Object obj) {
		if (obj instanceof SubsetObject) {
			Iterator it = ((SubsetObject) obj).getSubsets().iterator();
			while (it.hasNext()) {
				TermSubset cat = (TermSubset) it.next();
				scratch.add(cat.getName());
			}
		}
		return scratch;
	}
	
	public String getID() {
		return "category";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Category";
	}
}
