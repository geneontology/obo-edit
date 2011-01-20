package org.obo.filters;

import org.obo.datamodel.*;

import java.util.Iterator;

import org.apache.log4j.*;

/**
 * 
 * true if the object is a class, and can not be traced via an isa path to a class that has parents but no isa parent
 * AND has one or more of some other kind of parent.
 * 
 * here the 'isa path' is reflexive; ie if the tested object has parents and no isa parent then the criteria is true
 *
 */
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
			return true;
	}

	protected boolean isIsaComplete(LinkedObject lo) {
		// rootnodes
		if (lo.getParents().size() == 0)
			return true;
		Iterator it = lo.getParents().iterator();
		int n = 0;
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (link.getType().equals(OBOProperty.IS_A)) {
				if (isIsaComplete(link.getParent()))
					return true;
			}
			if (!link.getType().equals(OBOProperty.DISJOINT_FROM)) {
				n++;
			}
		}
		if (n == 0)
			return true;
		return false;
	}

	@Override
	public String toString() {
		return "Is isa complete";
	}
}
