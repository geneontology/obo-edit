package org.obo.filters;

import java.util.Iterator;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;

public class HasIsaParentCriterion extends AbstractBooleanCriterion {

	public String getID() {
		return "has_isa_parent";
	}

	public boolean matches(IdentifiedObject o) {
		if (o instanceof LinkedObject) {
			return hasIsaParent((LinkedObject) o);
		} else
			return false;
	}

	protected boolean hasIsaParent(LinkedObject lo) {
		if (lo.getParents().size() == 0)
			return false;
		Iterator it = lo.getParents().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (link.getType().equals(OBOProperty.IS_A)) {
					return true;
			}
		}
		return false;
	}

	@Override
	public String toString() {
		return "Has is_a parent";
	}
}
