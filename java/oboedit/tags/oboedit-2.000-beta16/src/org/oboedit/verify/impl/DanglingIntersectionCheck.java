package org.oboedit.verify.impl;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.*;

import java.util.*;

public class DanglingIntersectionCheck extends AbstractCheck implements
		OntologyCheck {

	@Override
	protected void initConfiguration() {
		configuration
				.setCondition((byte) (VerificationManager.LOAD | VerificationManager.MANUAL));
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
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (TermUtil.isIntersection(link)
					&& link.getParent() instanceof DanglingObject) {
				CheckWarning warning = new CheckWarning(
						"The cross product definition of " + object.getName()
								+ " (" + object.getID() + ") refers to a "
								+ "dangling parent " + link.getParent() + ". "
								+ "Dangling parents cannot be used by "
								+ "the reasoner, so this cross product "
								+ "definition will not be used.", false, this,
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
