package org.obo.identifier;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.util.IDUtil;

import org.apache.log4j.*;

public class DefaultIDGenerator implements IDGenerator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultIDGenerator.class);

	protected static List matchSequence(String s, List sequence, LinkedObject lo) {
		List out = new LinkedList();
		Iterator it = sequence.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof String) {
				if (!s.startsWith((String) o)) {
					return null;
				} else {
					s = s.substring(((String) o).length(), s.length());
				}
			} else if (o instanceof IDUtil.Variable) {
				IDUtil.Variable v = (IDUtil.Variable) o;
				if (v.getName().equals("sequence")) {
					int length = 0;
					if (v.getParams().size() > 0) {
						try {
							length = Integer.parseInt((String) v.getParams()
									.get(0));
						} catch (NumberFormatException ex) {
						}
					}

					if (length > s.length()) {
						length = s.length();
					}

					for (int i = 0; i < length; i++)
						if (!Character.isDigit(s.charAt(i))) {
							return null;
						}

					out.add(new IDUtil.VariableValue((IDUtil.Variable) o, s
							.substring(0, length)));
					s = s.substring(length, s.length());
				} else if (v.getName().equals("id")) {
					if (lo == null) {
						out.add(new IDUtil.VariableValue(
								(IDUtil.Variable) o, ""));
					} else {
						if (s.startsWith(lo.getID())) {
							s = s.substring(lo.getID().length(), s.length());
							out.add(new IDUtil.VariableValue(
									(IDUtil.Variable) o, lo.getID()));
						}
					}
				}
			}
		}
		return out;
	}

	protected IDProfile profile;
	protected IDProfile defaultProfile = new NamedIDProfile("<default profile>");

	public DefaultIDGenerator() {
		defaultProfile.setDefaultRule("id:$sequence(7)$");
	}

	public void setProfile(IDProfile profile) {
		this.profile = profile;
	}

	public IDProfile getProfile() {
		if (profile == null)
			return defaultProfile;
		else
			return profile;
	}

	public static String padNum(int val, int length) {
		StringBuffer out = new StringBuffer();
		out.append(val);
		for (int i = 1; i < length; i++) {
			if ((int) Math.pow(10, i) > val)
				out.insert(0, "0");
		}
		return out.toString();
	}

	public static String resolveVar(IDUtil.Variable v, List valueList,
			LinkedObject lo) {
		if (v.getName().equals("sequence")) {
			int lowVal = 0;
			int highVal = Integer.MAX_VALUE;
			int padVal = 8;
			if (v.getParams().size() > 0) {
				try {
					padVal = Integer.parseInt((String) v.getParams().get(0));
				} catch (NumberFormatException ex) {
				}
			}
			if (v.getParams().size() > 1) {
				try {
					lowVal = Integer.parseInt((String) v.getParams().get(1));
				} catch (NumberFormatException ex) {
				}
			}
			if (v.getParams().size() > 2) {
				try {
					highVal = Integer.parseInt((String) v.getParams().get(2));
				} catch (NumberFormatException ex) {
				}
			}
			int tentative = lowVal;
			Iterator it = valueList.iterator();
			while (it.hasNext()) {
				Integer io = (Integer) it.next();
				int val = io.intValue();
				if (val < tentative)
					continue;
				else if (val == tentative)
					tentative++;
				else if (val > tentative) {
					break;
				}
			}
			if (tentative > highVal)
				return null;

			return padNum(tentative, padVal);
		} else if (v.getName().equals("id")) {
			if (lo == null)
				return "";
			else
				return lo.getID();
		}
		return null;
	}
	
	public static Collection getIDs(OBOSession session) {
		Collection allReserved = new HashSet();

		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiableObject io = (IdentifiableObject) it.next();
			allReserved.add(io.getID());
			if (io instanceof MultiIDObject) {
				Iterator it2 = ((MultiIDObject) io).getSecondaryIDs()
						.iterator();
				while (it2.hasNext()) {
					String s = (String) it2.next();
					allReserved.add(s);
				}
			}
		}
		return allReserved;
	}
	
	public String generateID(OBOSession session, String currentRule, LinkedObject lo, Collection reservedIDs, boolean temporary) {
		Collection allReserved = getIDs(session);
		if (reservedIDs != null)
			allReserved.addAll(reservedIDs);
		return generateID(currentRule, lo, allReserved, temporary);
	}
	
	public String generateID(String currentRule,
			LinkedObject lo, Collection reservedIDs, boolean temporary) {
		List list = IDUtil.parseVarString(currentRule);
		if (list == null)
			return null;

		List matchList = new LinkedList();

		Iterator it;

		if (reservedIDs != null) {
			it = reservedIDs.iterator();
			while (it.hasNext()) {
				String id = (String) it.next();
				List seqMatch = matchSequence(id, list, lo);
				if (seqMatch != null)
					matchList.add(seqMatch);
			}
		}
/*
		it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			List seqMatch = matchSequence(io.getID(), list, lo);
			if (seqMatch != null)
				matchList.add(seqMatch);
			if (io instanceof MultiIDObject) {
				Iterator it2 = ((MultiIDObject) io).getSecondaryIDs()
						.iterator();
				while (it2.hasNext()) {
					String s = (String) it2.next();
					seqMatch = matchSequence(s, list, lo);
					if (seqMatch != null)
						matchList.add(seqMatch);
				}
			}
		}
*/
		it = list.iterator();
		StringBuffer out = new StringBuffer();
		int varIndex = 0;
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof String) {
				out.append((String) o);
			} else if (o instanceof IDUtil.Variable) {
				List valueList = new ArrayList();
				// Set mungeSet = new HashSet();
				Iterator it2 = matchList.iterator();
				while (it2.hasNext()) {
					List varMatches = (List) it2.next();
					IDUtil.VariableValue vv = (IDUtil.VariableValue) varMatches
							.get(varIndex);
					try {
						valueList.add(new Integer(vv.getValue()));
					} catch (NumberFormatException ex) {
					}
				}
				/*
				 * List mungeList = new ArrayList(); mungeList.addAll(mungeSet);
				 * Collections.sort(mungeList);
				 */
				Collections.sort(valueList);

				/***************************************************************
				 * PASS THE MUNGE LIST AND THE VARIABLE NUMBER TO THE SEQUENCE
				 * GENERATOR!!!
				 * 
				 * 
				 */
				String varVal = resolveVar((IDUtil.Variable) o, valueList, lo); 
				if (varVal == null)
					return null;
				out.append(varVal);
				varIndex++;
			}
		}
		return out.toString();
	}

	public String generateID(OBOSession session, LinkedObject lo,
			Collection reservedIDs, boolean temporary) {
		String currentRule = null;
		if (!temporary) {
			if (lo != null) {
				Iterator it = profile.getRules().iterator();
				while (it.hasNext()) {
					IDRule pair = (IDRule) it.next();
					if (pair.getFilter().satisfies(lo)) {
						currentRule = pair.getRule();
						break;
					}
				}
			}
			if (currentRule == null)
				currentRule = profile.getDefaultRule();
		} else
			currentRule = "__temp__$sequence(10)$__";
		return generateID(session, currentRule, lo, reservedIDs, temporary);
	}
}
