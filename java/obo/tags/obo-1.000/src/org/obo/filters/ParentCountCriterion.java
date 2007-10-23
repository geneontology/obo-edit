package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

public class ParentCountCriterion extends AbstractNumberCriterion {

	public Collection getValues(Collection scratch, Object obj) {
		IdentifiedObject o = (IdentifiedObject) obj;
		if (o instanceof LinkedObject && !TermUtil.isObsolete(o))
			scratch.add(new Integer(((LinkedObject) o).getParents().size()));
		return scratch;
	}

	public String getID() {
		return "parent_count";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Parent count";
	}
}
