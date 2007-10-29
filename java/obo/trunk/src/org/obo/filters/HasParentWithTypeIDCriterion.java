package org.obo.filters;

import java.util.Collection;
import java.util.Iterator;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;

public class HasParentWithTypeIDCriterion extends
		AbstractStringCriterion<IdentifiedObject> {

	public static final HasParentWithTypeIDCriterion CRITERION = new HasParentWithTypeIDCriterion();

	public Collection<String> getValues(Collection<String> scratch,
			IdentifiedObject obj) {
		if (obj instanceof LinkedObject) {
			for(Link link :((LinkedObject) obj).getParents()) {
				scratch.add(link.getType().getID());
			}
		}
		return scratch;
	}

	public int getMaxCardinality() {
		return Integer.MAX_VALUE;
	}

	public int getMinCardinality() {
		return 1;
	}

	public String getID() {
		return "has_parent_with_type_id";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Has parent with type id";
	}
}
