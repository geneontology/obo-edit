package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class IDSearchCriterion extends AbstractStringCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDSearchCriterion.class);

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
