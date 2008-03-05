package org.oboedit.verify.impl;

import java.util.*;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.*;
import org.oboedit.verify.*;

public class CycleCheck extends AbstractCheck implements OntologyCheck {

	@Override
	protected void initConfiguration() {
		configuration
				.setCondition((byte) (VerificationManager.SAVE
						| VerificationManager.REASONER_ACTIVATED | VerificationManager.MANUAL));
	}

	public Collection check(OBOSession history, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {

		Collection properties = new HashSet();
		Iterator it = TermUtil.getRelationshipTypes(history).iterator();
		while (it.hasNext()) {
			OBOProperty property = (OBOProperty) it.next();
			if (property.isTransitive() && !property.isCyclic())
				properties.add(property);
		}
		System.err.println("CycleCheck: properties = " + properties);
		List out = new LinkedList();
		if (currentObject != null) {
			if (currentObject instanceof LinkedObject)
				check((LinkedObject) currentObject, properties, out);
		} else {
			it = history.getObjects().iterator();
			for (int i = 0; it.hasNext(); i++) {
				Object o = it.next();
				int percentage = 100 * i / history.getObjects().size();
				setProgressValue(percentage);
				setProgressString("checking object " + (i + 1) + " of "
								+ history.getObjects().size());
				if (o instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) o;
					check(lo, properties, out);
					if (isCancelled() || out.size() > VerificationManager.MAX_WARNINGS)
						return out;
				}
			}
		}
		return out;
	}

	protected void check(LinkedObject object, Collection properties,
			List warnings) {
		Iterator it = properties.iterator();
		while (it.hasNext()) {
			OBOProperty property = (OBOProperty) it.next();
			LinkDatabase linkDatabase;
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
	 * if (lookedAt.contains(object)) { System.err.println("FOUND CYCLE AT
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
