package org.oboedit.verify.impl;

import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.VerificationManager;
import org.oboedit.verify.*;

import java.util.*;

public class NameRedundancyCheck extends AbstractCheck implements OntologyCheck {

	@Override
	protected void initConfiguration() {
		configuration.setCondition((byte) (VerificationManager.TEXT_EDIT_COMMIT
				| VerificationManager.SAVE | VerificationManager.MANUAL));
	}

	public Collection check(OBOSession session, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {
		List out = new LinkedList();
		if (currentObject == null) {
			Map nameMap = new HashMap();
			Iterator it = session.getObjects().iterator();
			for (int i = 0; it.hasNext(); i++) {
				IdentifiedObject io = (IdentifiedObject) it.next();
				int percentage = 50 * i / session.getObjects().size();
				setProgressValue(percentage);
				setProgressString("checking object " + (i + 1) + " of "
						+ session.getObjects().size());
				if (!checkObsoletes && TermUtil.isObsolete(io))
					continue;
				createMapping(nameMap, io.getName(), io);
				if (io instanceof SynonymedObject) {
					SynonymedObject so = (SynonymedObject) io;
					Iterator it2 = so.getSynonyms().iterator();
					while (it2.hasNext()) {
						Synonym s = (Synonym) it2.next();
						if (s.getScope() == Synonym.EXACT_SYNONYM)
							createMapping(nameMap, s.getText(), io);
					}
				}
			}
			it = nameMap.keySet().iterator();
			for (int j = 0; it.hasNext(); j++) {
				String name = (String) it.next();
				int percentage = 50 + (50 * j / nameMap.keySet().size());
				setProgressValue(percentage);
				setProgressString("checking object " + (j + 1) + " of "
						+ session.getObjects().size());
				Collection c = (Collection) nameMap.get(name);
				if (c.size() > 1) {
					StringBuffer termList = new StringBuffer();
					Iterator it2 = c.iterator();
					for (int i = 0; i < c.size(); i++) {
						IdentifiedObject io = (IdentifiedObject) it2.next();
						if (!checkObsoletes && TermUtil.isObsolete(io))
							continue;
						if (i > 0 && c.size() > 2)
							termList.append(", ");
						if (i == c.size() - 1)
							termList.append("and ");
						termList.append("<a href='file:" + io.getID() + "'>"
								+ io.getName() + " (" + io.getID() + ")</a>");
					}
					it2 = c.iterator();
					while (it2.hasNext()) {
						IdentifiedObject io = (IdentifiedObject) it2.next();
						if (!checkObsoletes && TermUtil.isObsolete(io))
							continue;
						CheckWarning warning = new CheckWarning(
								"The terms " + termList.toString()
										+ "share the name or synonym \"" + name
										+ "\".", false, this, io);
						out.add(warning);
						if (isCancelled()
								|| out.size() > VerificationManager.MAX_WARNINGS)
							return out;
					}
				}
			}
		} else {
			if (!checkObsoletes && TermUtil.isObsolete(currentObject))
				return Collections.EMPTY_LIST;
			Iterator it;
			Collection names = new HashSet();
			if (currentObject instanceof SynonymedObject) {
				it = ((SynonymedObject) currentObject).getSynonyms().iterator();
				while (it.hasNext()) {
					Synonym s = (Synonym) it.next();
					if (s.getScope() == Synonym.EXACT_SYNONYM)
						names.add(s.getText());
				}
			}

			it = session.getObjects().iterator();
			while (it.hasNext()) {
				IdentifiedObject io = (IdentifiedObject) it.next();
				if (io.equals(currentObject))
					continue;
				if (!checkObsoletes && TermUtil.isObsolete(io))
					continue;
				if (io.getName().equals(currentObject.getName())) {
					CheckWarning warning = new CheckWarning(
							"The current term has the same name " + "as "
									+ io.getID(), false, this, currentObject);
					out.add(warning);
					if (out.size() > VerificationManager.MAX_WARNINGS)
						return out;
				}

				if (io instanceof SynonymedObject) {
					Iterator it2 = ((SynonymedObject) io).getSynonyms()
							.iterator();
					while (it2.hasNext()) {
						Synonym s = (Synonym) it2.next();
						if (s.getScope() == Synonym.EXACT_SYNONYM
								&& s.getText().equals(currentObject.getName())) {
							CheckWarning warning = new CheckWarning(
									"The current term name is "
											+ "the same as a " + "synonym of "
											+ io.getName() + " (" + io.getID()
											+ ")", false, this, currentObject);
							out.add(warning);
							if (isCancelled()
									|| out.size() > VerificationManager.MAX_WARNINGS)
								return out;
						}
						/*
						 * if (names.contains(s.getText())) { CheckWarning
						 * warning = new CheckWarning("The current term shares a "+
						 * "synonym with "+io.getName()+ " ("+io.getID()+")",
						 * false, this, currentObject); out.add(warning); }
						 */
					}
					if (names.contains(io.getName())) {
						CheckWarning warning = new CheckWarning(
								"The current term synonym " + "\""
										+ io.getName() + "\" is "
										+ "in use as the term name of "
										+ io.getID(), false, this,
								currentObject);
						out.add(warning);
						if (out.size() > VerificationManager.MAX_WARNINGS)
							return out;
					}
				}
			}
		}
		return out;
	}

	protected void createMapping(Map nameMap, String name, IdentifiedObject io) {
		Collection c = (Collection) nameMap.get(name);
		if (c == null) {
			c = new HashSet();
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
