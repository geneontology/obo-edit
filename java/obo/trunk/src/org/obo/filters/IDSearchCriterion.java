package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public class IDSearchCriterion extends AbstractStringCriterion {

	public Collection getValues(Collection scratch, Object obj) {
		scratch.add(((IdentifiedObject) obj).getID());
		if (obj instanceof MultiIDObject) {
			Iterator it = ((MultiIDObject) obj).getSecondaryIDs().iterator();
			while (it.hasNext()) {
				scratch.add(it.next());
			}
		}
		return scratch;
	}

	public String getID() {
		return "id";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "ID";
	}
}
