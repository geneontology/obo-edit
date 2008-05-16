package org.oboedit.verify.impl;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.util.ReasonerUtil;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.*;

import org.apache.log4j.*;

public class DisjointednessCheck extends AbstractCheck implements OntologyCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DisjointednessCheck.class);

	public static final int MAX_BAD_ITEMS = 100;

	@Override
	protected void initConfiguration() {
		configuration
				.setCondition((byte) (VerificationManager.SAVE
						| VerificationManager.REASONER_ACTIVATED | VerificationManager.MANUAL));
	}

	@Override
	public boolean needsReasoner() {
		return true;
	}

	public Collection check(OBOSession history, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {

		Set out = new HashSet();

		int badItems = 0;
		Iterator it = history.getObjects().iterator();
		for (int i = 0; badItems < MAX_BAD_ITEMS && it.hasNext(); i++) {
			Object o = it.next();
			boolean isBad = false;
			int percentage = 100 * i / history.getObjects().size();
			setProgressValue(percentage);
			setProgressString("checking object " + (i + 1) + " of "
					+ history.getObjects().size());

			if (o instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) o;
				Collection superClasses = new HashSet();
				Iterator it2 = linkDatabase.getParents(lo).iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (link.getParent() instanceof OBOClass
							&& linkDatabase.isSubPropertyOf(link.getType(),
									OBOProperty.IS_A)) {
						superClasses.add(link.getParent());
					}
				}
				it2 = superClasses.iterator();
				while (it2.hasNext()) {
					OBOClass oboClass = (OBOClass) it2.next();
					Iterator it3 = superClasses.iterator();
					while (it3.hasNext()) {
						OBOClass oboClass2 = (OBOClass) it3.next();
						if (ReasonerUtil.isDisjoint(linkDatabase, oboClass,
								oboClass2, false)) {
							if (!isBad)
								badItems++;
							isBad = true;
							CheckWarning warning = new CheckWarning(lo
									.getName()
									+ " ("
									+ lo.getID()
									+ ") "
									+ "has disjoint superclasses "
									+ oboClass
									+ " and " + oboClass2, true, this, lo);
							out.add(warning);
							if (isCancelled()
									|| out.size() > VerificationManager.MAX_WARNINGS)
								return out;
						}
					}
				}
			}
		}
		return out;
	}

	/*
	 * protected void check(LinkedObject object, Collection properties, List
	 * warnings) { Iterator it = properties.iterator(); while(it.hasNext()) {
	 * OBOProperty property = (OBOProperty) it.next();
	 * 
	 * if (TermUtil.isCycle(Controller.getController().
	 * getCurrentLinkDatabase(), property, object)) { CheckWarning warning = new
	 * CheckWarning(object.getName()+" ("+object.getID()+") "+ "is part of a
	 * cycle over the property "+ property+". ", true, this, object);
	 * warnings.add(warning); } } }
	 */

	@Override
	public String getDescription() {
		return "Disjointedness Check";
	}

	public String getID() {
		return "DISJOINTEDNESS_CHECK";
	}
}
