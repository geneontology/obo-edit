package org.oboedit.verify.impl;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.*;

import java.util.*;

import org.apache.log4j.*;

public class DanglingIntersectionCheck extends AbstractCheck implements
	OntologyCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DanglingIntersectionCheck.class);

	@Override
	protected void initConfiguration() {
		configuration
				.setCondition((byte) (VerificationManager.LOAD | VerificationManager.MANUAL));
	}

	public Collection<CheckWarning> check(OBOSession history, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {
		List<CheckWarning> out = new LinkedList<CheckWarning>();
		if (currentObject != null) {
			if (currentObject instanceof LinkedObject)
				check((LinkedObject) currentObject, out);
		} else {
			Iterator<IdentifiedObject> it = history.getObjects().iterator();
			while (it.hasNext()) {
				IdentifiedObject o = it.next();
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

	protected void check(LinkedObject object, List<CheckWarning> warnings) {
		Iterator<Link> it = object.getParents().iterator();
		while (it.hasNext()) {
			Link link = it.next();
			if (TermUtil.isIntersection(link)
					&& link.getParent() instanceof DanglingObject) {
				CheckWarning warning = new CheckWarning(
						"The cross product definition of " + object.getName()
								+ " (" + object.getID() + ") refers to a "
								+ "dangling parent " + link.getParent() + ". ", false, this,
						object);
				warnings.add(warning);
				if (isCancelled()
						|| warnings.size() > VerificationManager.MAX_WARNINGS)
					return;
			}
		}
	}

	@Override
	public String getDescription() {
		return "Dangling Intersection Check";
	}

	public String getID() {
		return "DANGLING_INTERSECTION_CHECK";
	}
}
