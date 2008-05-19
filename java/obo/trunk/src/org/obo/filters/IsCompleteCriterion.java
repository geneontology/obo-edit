package org.obo.filters;

import org.obo.datamodel.*;

import java.util.Iterator;

import org.apache.log4j.*;

public class IsCompleteCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsCompleteCriterion.class);

	public String getID() {
		return "is_intersection";
	}

	public boolean matches(IdentifiedObject o) {
		if (o instanceof LinkedObject) {
			Iterator it = ((LinkedObject) o).getParents().iterator();
			boolean found = false;
			while (it.hasNext()) {
				Link l = (Link) it.next();
				if (l instanceof OBORestriction
						&& ((OBORestriction) l).completes()) {
					found = true;
					break;
				}
			}
			return found;
		} else
			return false;
	}

	@Override
	public String toString() {
		return "Is intersection";
	}
}
