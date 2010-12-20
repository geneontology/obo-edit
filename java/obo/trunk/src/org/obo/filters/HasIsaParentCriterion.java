package org.obo.filters;

import java.util.Iterator;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;

import org.apache.log4j.*;

public class HasIsaParentCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HasIsaParentCriterion.class);

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
		for(Link link : lo.getParents()){
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
