package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class IsaParentCountCriterion extends AbstractNumberCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsaParentCountCriterion.class);

	public Collection getValues(Collection scratch, Object obj) {
		IdentifiedObject o = (IdentifiedObject) obj;
		int count = 0;
		if (o instanceof LinkedObject && !TermUtil.isObsolete(o)) {
			for(Link link : ((LinkedObject) o).getParents()) {
				if (link.getType().equals(OBOProperty.IS_A))
					count++;
			}
		}
		scratch.add(new Integer(count));
		return scratch;
	}

	public String getID() {
		return "isa_parent_count";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "ISA Parent count";
	}
}
