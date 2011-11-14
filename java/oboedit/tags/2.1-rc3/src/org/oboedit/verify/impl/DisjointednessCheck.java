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

	public Collection<CheckWarning> check(OBOSession history, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {

		Set<CheckWarning> out = new HashSet<CheckWarning>();
		int badItems = 0;
		int historyObjsSize = history.getObjects().size();

		Iterator<IdentifiedObject> it = history.getObjects().iterator();
		for (int i = 0; badItems < MAX_BAD_ITEMS && it.hasNext(); i++) {
			IdentifiedObject historyObject = it.next();
			boolean isBad = false;
			int percentage = 100 * i / historyObjsSize;
			setProgressValue(percentage);
			setProgressString("checking object " + (i + 1) + " of " + historyObjsSize);

			if (historyObject instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) historyObject;
				Collection<LinkedObject> superClasses = new HashSet<LinkedObject>();

				for(Link link : linkDatabase.getParents(lo)){
					if (link.getParent() instanceof OBOClass
							&& linkDatabase.isSubPropertyOf(link.getType(), OBOProperty.IS_A)) {
						superClasses.add(link.getParent());
					}
				}

				for(Object superClassObj1 : superClasses){
					OBOClass oboClass1 = (OBOClass) superClassObj1;
					for(Object superClassObj2 : superClasses){
						OBOClass oboClass2 = (OBOClass) superClassObj2;
						if (ReasonerUtil.isDisjoint(linkDatabase, oboClass1, oboClass2, false)) {
							if (!isBad)
								badItems++;
							isBad = true;
							CheckWarning warning = new CheckWarning(lo
									.getName()
									+ " ("
									+ lo.getID()
									+ ") "
									+ "has disjoint superclasses "
									+ oboClass1
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


	@Override
	public String getDescription() {
		return "Disjointedness Check";
	}

	public String getID() {
		return "DISJOINTEDNESS_CHECK";
	}
}
