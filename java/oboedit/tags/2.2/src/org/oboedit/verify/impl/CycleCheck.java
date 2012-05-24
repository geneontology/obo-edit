package org.oboedit.verify.impl;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.*;

import org.apache.log4j.*;

public class CycleCheck extends AbstractCheck implements OntologyCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CycleCheck.class);

	@Override
	protected void initConfiguration() {
		configuration
				.setCondition((byte) (VerificationManager.SAVE
                                                      // If it's reasoner activated, it also runs on load
                                                      // (yes, even if the reasoner's off).
                                                      // | VerificationManager.REASONER_ACTIVATED
                                                      | VerificationManager.MANUAL));
	}

	public Collection<CheckWarning> check(OBOSession history, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {

            // properties not currently used
// 		Collection<OBOProperty> properties = new HashSet<OBOProperty>();
// 		Iterator<OBOProperty> it = TermUtil.getRelationshipTypes(history).iterator();
// 		while (it.hasNext()) {
// 			OBOProperty property = it.next();
// 			if (property.isTransitive() && !property.isCyclic())
// 				properties.add(property);
// 		}
// //		logger.info("CycleCheck: properties = " + properties);
		List<CheckWarning> out = new LinkedList<CheckWarning>();
		if (currentObject != null) {
			if (currentObject instanceof LinkedObject)
                            // check((LinkedObject) currentObject, properties, out);
                            checkForCycles((LinkedObject) currentObject, out);
		} else {
			Iterator<IdentifiedObject> ith = history.getObjects().iterator();
			for (int i = 0; ith.hasNext(); i++) {
				Object o = ith.next();
				int percentage = 100 * i / history.getObjects().size();
				setProgressValue(percentage);
				setProgressString("checking object " + (i + 1) + " of "
								+ history.getObjects().size());
				if (o instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) o;
					// check(lo, properties, out);
                                        checkForCycles(lo, out);
                                        // logger.debug("CycleCheck: lo = " + lo + ", out = " + out); // DEL
					if (isCancelled() || out.size() > VerificationManager.MAX_WARNINGS)
						return out;
				}
			}
		}
		return out;
	}

	protected void checkForCycles(LinkedObject object, List<CheckWarning> warnings) {
            for (Link link : object.getChildren()) {
                LinkedObject child = link.getChild();
                // If object is a descendant of its own child, then we have a cycle
                // (possibly a mixed-relationship cycle)
                boolean isDesc = TermUtil.isDescendant(child, object);
                // logger.debug("checkForCycles: obj = " + object + ", child = " + child + ", isDescendent = " + isDesc);
                if (isDesc) {
                    CheckWarning warning = new CheckWarning(object.getName() + " ("
                                                            + object.getID() + ") "
                                                            + "is part of a cycle.",
                                                            // false means this is not a fatal error
                                                            false, this, object);
                    warnings.add(warning);
		    return; // Don't need to check any more children
                }
            }
        }

    // I don't think this method is currently used--it's been replaced by
    // checkForCycles, above.
	protected void check(LinkedObject object, Collection<OBOProperty> properties,
			List<CheckWarning> warnings) {
		Iterator<OBOProperty> it = properties.iterator();
		while (it.hasNext()) {
			OBOProperty property = it.next();
			if (TermUtil.isCycle(SessionManager.getManager()
					.getCurrentFullLinkDatabase(), property, object)) {
				CheckWarning warning = new CheckWarning(object.getName() + " ("
						+ object.getID() + ") "
						+ "is part of a cycle over the property " + property
						+ ". ", true, this, object);
				warnings.add(warning);
				if (warnings.size() > VerificationManager.MAX_WARNINGS)
					return;
			}
		}
	}

	/*
	 * protected boolean checkCycle(LinkedObject object, Collection lookedAt) {
	 * if (lookedAt.contains(object)) { logger.info("FOUND CYCLE AT
	 * "+object); return true; } lookedAt.add(object); Iterator it =
	 * Controller.getController().getCurrentLinkDatabase().
	 * getParents(object).iterator(); while(it.hasNext()) { Link link = (Link)
	 * it.next(); if (link.getType().equals(OBOProperty.IS_A)) { if
	 * (checkCycle(link.getParent(), lookedAt)) return true; } } return false; }
	 */
	@Override
	public String getDescription() {
		return "Cycle Check";
	}

	public String getID() {
		return "CYCLE_CHECK";
	}
}
