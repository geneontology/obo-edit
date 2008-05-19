package org.obo.filters;

import org.obo.datamodel.*;

import java.util.Iterator;

import org.apache.log4j.*;

public class IsaCompleteCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsaCompleteCriterion.class);

	public String getID() {
		return "is_isa_complete";
	}

	public boolean matches(IdentifiedObject o) {
		if (o instanceof LinkedObject) {
			return isIsaComplete((LinkedObject) o);
		} else
			return false;
	}

	protected boolean isIsaComplete(LinkedObject lo) {
		if (lo.getParents().size() == 0)
			return true;
		Iterator it = lo.getParents().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (link.getType().equals(OBOProperty.IS_A)) {
				if (isIsaComplete(link.getParent()))
					return true;
			}
		}
		return false;
	}

	@Override
	public String toString() {
		return "Is isa complete";
	}
}
