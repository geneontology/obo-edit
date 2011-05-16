package org.oboedit.verify.impl;

/** Checks whether any term has exactly one intersection_of line */

import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.*;

import java.util.*;

import org.apache.log4j.*;

public class SingleIntersectionCheck extends AbstractCheck implements OntologyCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SingleIntersectionCheck.class);

	@Override
	protected void initConfiguration() {
	    configuration.setCondition((byte) (VerificationManager.LOAD | VerificationManager.MANUAL | VerificationManager.SAVE));
	}

	public Collection check(OBOSession history, IdentifiedObject currentObject,
				byte condition, boolean checkObsoletes) {
		List out = new LinkedList();
		if (currentObject != null) {
			if (currentObject instanceof LinkedObject)
				check((LinkedObject) currentObject, out);
		} else {
			Iterator it = history.getObjects().iterator();
			while (it.hasNext()) {
				Object o = it.next();
				if (o instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) o;
					check(lo, out);
					if (isCancelled()
							|| out.size() > VerificationManager.MAX_WARNINGS)
						return out;
				}
			}
		}
		return out;
	}

	protected void check(LinkedObject object, List warnings) {
		Iterator it = object.getParents().iterator();
		int intersectionLinks = 0;
		while (it.hasNext()) {
		    Link link = (Link) it.next();
		    if (TermUtil.isIntersection(link))
			++intersectionLinks;
		}
		if (intersectionLinks == 1) {
		    CheckWarning warning = new CheckWarning(object.getName() + " has exactly one intersection_of link",
							    false, this, object);
		    warnings.add(warning);
		    if (isCancelled()
			|| warnings.size() > VerificationManager.MAX_WARNINGS)
			return;
		}
	}

	@Override
	public String getDescription() {
		return "Single Intersection Check";
	}

	public String getID() {
		return "SINGLE_INTERSECTION_CHECK";
	}
}
