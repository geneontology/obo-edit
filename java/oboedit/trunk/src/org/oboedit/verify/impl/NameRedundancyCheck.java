package org.oboedit.verify.impl;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.*;

import java.util.*;

import org.apache.log4j.*;

public class NameRedundancyCheck extends AbstractCheck implements OntologyCheck {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NameRedundancyCheck.class);

	@Override
	protected void initConfiguration() {
		configuration.setCondition((byte) (VerificationManager.TEXT_EDIT_COMMIT | VerificationManager.TEXT_EDIT_THREAD
				| VerificationManager.SAVE | VerificationManager.MANUAL));
	}

	public Collection<CheckWarning> check(OBOSession session, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {
		List<CheckWarning> out = new LinkedList<CheckWarning>();
		if (currentObject == null) {
			Map<String, Collection<IdentifiedObject>> nameMap = new HashMap<String, Collection<IdentifiedObject>>();
			Iterator<IdentifiedObject> it = session.getObjects().iterator();
			for (int i = 0; it.hasNext(); i++) {
				IdentifiedObject io = it.next();
				int percentage = 50 * i / session.getObjects().size();
				setProgressValue(percentage);
				setProgressString("checking object " + (i + 1) + " of "
						+ session.getObjects().size());
				if (!checkObsoletes && TermUtil.isObsolete(io))
					continue;
				createMapping(nameMap, io.getName(), io);
                                out.addAll(checkForRedundancy(session, io, condition, checkObsoletes));
				if (io instanceof SynonymedObject) {
					SynonymedObject so = (SynonymedObject) io;
					Iterator<Synonym> it2 = so.getSynonyms().iterator();
					while (it2.hasNext()) {
						Synonym s = it2.next();
						if (s.getScope() == Synonym.EXACT_SYNONYM)
							createMapping(nameMap, s.getText(), io);
					}
				}
			}
			Iterator<String> its = nameMap.keySet().iterator();
			for (int j = 0; its.hasNext(); j++) {
				String name = its.next();
				int percentage = 50 + (50 * j / nameMap.keySet().size());
				setProgressValue(percentage);
				setProgressString("checking object " + (j + 1) + " of "
						+ session.getObjects().size());
				Collection<IdentifiedObject> c = nameMap.get(name);
				if (c.size() > 1) { // More than one term has this as its name or synonym
					StringBuffer termList = new StringBuffer();
					Iterator<IdentifiedObject> it2 = c.iterator();
					for (int i = 0; i < c.size(); i++) {
						IdentifiedObject io = it2.next();
						if (!checkObsoletes && TermUtil.isObsolete(io))
							continue;
						if (i > 0 && c.size() > 2)
							termList.append(", ");
						if (i == c.size() - 1)
							termList.append(" and ");
						termList.append(" <a href='file:" + io.getID() + "'>"
								+ io.getName() + " (" + io.getID() + ")</a>");
					}
					it2 = c.iterator();
					while (it2.hasNext()) {
						IdentifiedObject io = it2.next();
						if (!checkObsoletes && TermUtil.isObsolete(io))
							continue;
						CheckWarning warning = new CheckWarning(
								"The terms " + termList.toString()
										+ " share the name or synonym \"" + name
										+ "\".", false, this, io);
						out.add(warning);
						if (isCancelled()
								|| out.size() > VerificationManager.MAX_WARNINGS)
							return out;
					}
				}
                                //                                checkForRedundancy(c.last()); // ?
                                //                                checkForRedundancy(session, (IdentifiedObject) c.last(), condition, checkObsoletes);
			}
		} else { // currentObject != null
			if (!checkObsoletes && TermUtil.isObsolete(currentObject))
				return Collections.emptyList();
                        return checkForRedundancy(session, currentObject, condition, checkObsoletes);
                }
		return out;
	}

	public Collection<CheckWarning> checkForRedundancy(OBOSession session, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {
            List<CheckWarning> out = new LinkedList<CheckWarning>();

            // First check if a synonym of this term is the same as the term name
            for(Synonym s : ((SynonymedObject) currentObject).getSynonyms()) {
                if (s.getScope() == Synonym.EXACT_SYNONYM
                    && s.getText().equals(currentObject.getName())) {
                    CheckWarning warning = new CheckWarning(
                                                            "The term "
                                                            + "<a href='file:" + currentObject.getID() + "'>"
                                                            + currentObject.getName() + " (" + currentObject.getID()
                                                            + "</a>) has a synonym with the identical name.",
                                                            false, this, currentObject);
                    //                    logger.debug("warning: " + warning);
                    out.add(warning);
                    if (isCancelled()
                        || out.size() > VerificationManager.MAX_WARNINGS)
                        return out;
                }
            }


            Collection<String> names = new HashSet<String>();
            if (currentObject instanceof SynonymedObject) {
                Iterator<Synonym> it = ((SynonymedObject) currentObject).getSynonyms().iterator();
                while (it.hasNext()) {
                    Synonym s = it.next();
                    if (s.getScope() == Synonym.EXACT_SYNONYM)
                        names.add(s.getText());
                }
            }

            Iterator<IdentifiedObject> it = session.getObjects().iterator();
            while (it.hasNext()) {
                IdentifiedObject io = it.next();
                if (io.equals(currentObject))
                    continue;
                if (!checkObsoletes && TermUtil.isObsolete(io))
                    continue;
                // Dangling objects have null names
                String ioName = (io.getName() == null) ? "" : io.getName();
                if (ioName.equals(currentObject.getName())) {
                    CheckWarning warning = new CheckWarning("The term <a href='file:" + currentObject.getID() + "'>"
                                                            + currentObject.getName() + " (" + currentObject.getID() + ")</a>"
                                                            + " has the same name as "
                                                            + "<a href='file:" + io.getID() + "'>"
                                                            + io.getID() + "</a>",
                                                            false, this, currentObject);
                    out.add(warning);
                    if (out.size() > VerificationManager.MAX_WARNINGS)
                        return out;
                }

                if (io instanceof SynonymedObject) {
                    if (names.contains(io.getName())) {
                        CheckWarning warning = new CheckWarning(
								"The synonym " + "\""
                                                                + io.getName() + "\" of "
                                                                + "<a href='file:" + currentObject.getID() + "'>"
                                                                + currentObject.getName() + " (" + currentObject.getID() + ")</a>"
                                                                + " is also the term name of "
                                                                + "<a href='file:" + io.getID() + "'>"
                                                                + io.getID() + "</a>",
                                                                false, this,
								currentObject);
                        //                        logger.debug("Other warning: " + warning);
                        out.add(warning);
                        if (out.size() > VerificationManager.MAX_WARNINGS)
                            return out;
                    }
                }
            }

            return out;
        }

	protected void createMapping(Map<String, Collection<IdentifiedObject>> nameMap, String name, IdentifiedObject io) {
		Collection<IdentifiedObject> c = nameMap.get(name);
		if (c == null) {
			c = new HashSet<IdentifiedObject>();
			nameMap.put(name, c);
		}
		c.add(io);
	}

	@Override
	public String getDescription() {
		return "Name Redundancy Check";
	}

	public String getID() {
		return "NAME_REDUNDANCY_CHECK";
	}
}
