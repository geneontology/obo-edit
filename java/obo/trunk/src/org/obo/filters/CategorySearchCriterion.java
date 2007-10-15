package org.obo.filters;

/*
 * Returns the names of all the categories to which a given
 * {@link org.oboedit.datamodel.IdentifiedObject }
 * belongs.
 *
 */

import java.util.*;

import org.obo.datamodel.*;

public class CategorySearchCriterion extends AbstractStringCriterion {

	public Collection getValues(Collection scratch, Object obj) {
		if (obj instanceof CategorizedObject) {
			Iterator it = ((CategorizedObject) obj).getCategories().iterator();
			while (it.hasNext()) {
				TermCategory cat = (TermCategory) it.next();
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
