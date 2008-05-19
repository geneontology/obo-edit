package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class ParentCountCriterion extends AbstractNumberCriterion<IdentifiedObject> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ParentCountCriterion.class);
	
	public Collection getValues(Collection scratch, IdentifiedObject obj) {
		IdentifiedObject o = (IdentifiedObject) obj;
		if (o instanceof LinkedObject && !TermUtil.isObsolete(o))
			scratch.add(new Integer(((LinkedObject) o).getParents().size()));
		return scratch;
	}

	public String getID() {
		return "parent_count";
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Parent count";
	}
}
