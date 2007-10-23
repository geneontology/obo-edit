package org.obo.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.bbop.util.ObjectUtil;
import org.bbop.util.VectorFilter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MultiIDObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.SecondaryIDHistoryItem;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.DefaultIDResolution;
import org.obo.identifier.DefaultIDWarning;
import org.obo.identifier.DefaultLinkIDResolution;
import org.obo.identifier.DefaultLinkIDWarning;
import org.obo.identifier.IDGenerator;
import org.obo.identifier.IDProfile;
import org.obo.identifier.IDResolution;
import org.obo.identifier.IDRule;
import org.obo.identifier.IDWarning;
import org.obo.identifier.LinkIDResolution;
import org.obo.identifier.LinkIDWarning;
import org.obo.identifier.UnresolvedIDsException;
import org.obo.identifier.IDWarning.WarningType;

public class IDUtil {

	public static class VariableValue {
		protected IDUtil.Variable v;

		protected String value;

		public VariableValue(IDUtil.Variable v, String value) {
			this.v = v;
			this.value = value;
		}

		public IDUtil.Variable getVariable() {
			return v;
		}

		public String getValue() {
			return value;
		}
	}

	public static class Variable {
		protected String s;

		protected java.util.List params = new ArrayList();

		public Variable(String s) {
			this.s = s;
		}

		public String getName() {
			return s;
		}

		public void addParam(String param) {
			params.add(param);
		}

		public java.util.List getParams() {
			return params;
		}

		@Override
		public String toString() {
			return "[variable: " + s + ", params: " + params + "]";
		}
	}

	public static java.util.List parseVarString(String s) {
		java.util.List out = new LinkedList();
		if (s == null)
			return out;
		StringBuffer buffer = new StringBuffer();
		boolean inVar = false;
		boolean inParens = false;
		boolean inQuotes = false;
		IDUtil.Variable currentVariable = null;
		for (int i = 0; i < s.length(); i++) {
			if (!inParens && s.charAt(i) == '$') {
				if (inVar) {
					if (currentVariable == null)
						currentVariable = new IDUtil.Variable(buffer.toString());
					out.add(currentVariable);
					inVar = false;
					inParens = false;
					currentVariable = null;
				} else {
					out.add(buffer.toString());
					inVar = true;
					inParens = false;
				}
				buffer = new StringBuffer();
			} else if (inVar && !inParens && s.charAt(i) == '(') {
				currentVariable = new IDUtil.Variable(buffer.toString());
				buffer = new StringBuffer();
				inParens = true;
			} else if (inVar && inParens && s.charAt(i) == ')') {
				currentVariable.addParam(buffer.toString().trim());
				buffer = new StringBuffer();
				inParens = false;
			} else if (inVar && inParens && s.charAt(i) == ',') {
				currentVariable.addParam(buffer.toString().trim());
				buffer = new StringBuffer();
			} else if (s.charAt(i) == '\\') {
				if (i + 1 < s.length()
						&& (s.charAt(i + 1) == '$' || s.charAt(i + 1) == ')'
								|| s.charAt(i + 1) == '(' || s.charAt(i + 1) == ',')) {
					i++;
					buffer.append('$');
				}
			} else
				buffer.append(s.charAt(i));
		}
		if (inVar || inParens || inQuotes || currentVariable != null)
			return null;
		if (buffer.length() > 0)
			out.add(buffer.toString());
		return out;
	}

	/**
	 * Searches a HistoryList for items that require id remapping before the
	 * item can be applied, and returns a {@link Map} of ids to
	 * {@link Collection}s of ids to which they should be remapped
	 */
	public static Map createIDRemapping(HistoryList list) {
		Map out = new HashMap();
		VectorFilter destroyFilter = new VectorFilter() {
			public boolean satisfies(Object o) {
				return o instanceof DestroyObjectHistoryItem;
			}
		};
		final Collection destroyedIDs = new HashSet();
		Collection matches = HistoryUtil.findMatchingItems(list, destroyFilter);
		Iterator it = matches.iterator();
		while (it.hasNext()) {
			DestroyObjectHistoryItem item = (DestroyObjectHistoryItem) it
					.next();
			destroyedIDs.add(item.getTarget());
		}
		VectorFilter secondaryFilter = new VectorFilter() {
			public boolean satisfies(Object o) {
				return o instanceof SecondaryIDHistoryItem
						&& destroyedIDs.contains(((SecondaryIDHistoryItem) o)
								.getSecondaryID());
			}
		};
		matches = HistoryUtil.findMatchingItems(list, secondaryFilter);
		it = matches.iterator();
		while (it.hasNext()) {
			SecondaryIDHistoryItem item = (SecondaryIDHistoryItem) it.next();
			Collection c = (Collection) out.get(item.getSecondaryID());
			if (c == null) {
				c = new HashSet();
				out.put(item.getSecondaryID(), c);
			}
			c.add(item.getTarget());
		}
		return out;
	}

	public static boolean isLegalID(String id) {
		for (int i = 0; i < id.length(); i++)
			if (Character.isWhitespace(id.charAt(i)))
				return false;
		return true;
	}

	public static String[] fetchIDs(IDGenerator idGen, OBOSession session,
			LinkedObject parent, int size) {
		String[] out = new String[size];
		Set<String> reserved = new HashSet<String>();
		for (int i = 0; i < size; i++) {
			out[i] = IDUtil.fetchID(idGen, session, parent, reserved, false);
			reserved.add(out[i]);
		}
		return out;
	}

	public static String fetchID(IDGenerator idGen, OBOSession session,
			LinkedObject parent) {
		return IDUtil.fetchID(idGen, session, parent, null);
	}

	public static String fetchID(IDGenerator idGen, OBOSession session,
			LinkedObject parent, Collection<String> reservedIDs) {
		return IDUtil.fetchID(idGen, session, parent, reservedIDs, false);
	}

	public static String fetchID(IDGenerator idGen, OBOSession session,
			LinkedObject parent, Collection<String> reservedIDs,
			boolean temporary) {
		return idGen.generateID(session, parent, reservedIDs, temporary);
	}
	
	public static String fetchTemporaryID(OBOSession session) {
		return fetchTemporaryID(new DefaultIDGenerator(), session);
	}

	public static String fetchTemporaryID(IDGenerator idGen, OBOSession session) {
		return fetchID(idGen, session, null, null, true);
	}

	public static boolean equals(IDProfile a, IDProfile b) {

		boolean failed = false;
		if (a == b)
			return true;
		else if (a == null || b == null)
			return true;
		else {
			if (a.getRules().size() != b.getRules().size()
					&& a.getRules().size() != 0) {
				failed = true;
			} else {
				Iterator it = a.getRules().iterator();
				while (it.hasNext()) {
					IDRule rule = (IDRule) it.next();

					boolean found = false;

					Iterator it2 = b.getRules().iterator();
					while (it2.hasNext()) {
						IDRule rule2 = (IDRule) it2.next();
						if (rule.equals(rule2)) {
							found = true;
							break;
						}
					}
					if (!found) {
						failed = true;
						break;
					}
				}
			}
			if (!ObjectUtil.equals(a.getDefaultRule(), b.getDefaultRule()))
				failed = true;
		}
		return !failed;
	}

	public static List<HistoryItem> updateIDs(OBOSession session,
			Collection<LinkIDResolution> resolutions, boolean applyImmediately)
			throws UnresolvedIDsException {
		MultiMap<String, IdentifiedObject> secondaryIDMap = new MultiHashMap<String, IdentifiedObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (!TermUtil.isDangling(io) && io instanceof MultiIDObject) {
				for (String id : ((MultiIDObject) io).getSecondaryIDs()) {
					secondaryIDMap.add(id, io);
				}
			}
		}

		MultiMap<Link, LinkIDResolution> resolutionMap = new MultiHashMap<Link, LinkIDResolution>();
		if (resolutions != null) {
			for (LinkIDResolution r : resolutions) {
				resolutionMap.add(r.getLink(), r);
			}
		}

		LinkedList<LinkIDWarning> warnings = new LinkedList<LinkIDWarning>();
		Iterator<Link> linkIterator = TermUtil.getAllLinks(session
				.getLinkDatabase());
		while (linkIterator.hasNext()) {
			Link link = linkIterator.next();

			LinkIDWarning warning = getWarning(link, session, secondaryIDMap);
			if (warning != null) {
				Collection<LinkIDResolution> linkResolutions = resolutionMap
						.get(link);
				boolean parentProblem = warning.getParentWarning() != null;
				boolean typeProblem = warning.getTypeWarning() != null;
				if (parentProblem || typeProblem) {
					for (LinkIDResolution res : linkResolutions) {
						if (warning.getParentWarning() != null
								&& res.getParentResolution() != null) {
							parentProblem = false;
						}
						if (warning.getTypeWarning() != null
								&& res.getTypeResolution() != null) {
							typeProblem = false;
						}
						if (!(typeProblem && parentProblem))
							break;
					}
					if (parentProblem || typeProblem)
						warnings.add(warning);
				}
			}
		}
		if (warnings.size() > 0)
			throw new UnresolvedIDsException(warnings);
		List<HistoryItem> historyItems = new LinkedList<HistoryItem>();
		for (LinkIDResolution resolution : resolutions) {
			HistoryItem delItem = new DeleteLinkHistoryItem(resolution
					.getLink());
			Link newLink = (Link) resolution.getLink().clone();
			if (resolution.getParentResolution() != null) {
				IdentifiedObject io = session.getObject(resolution
						.getParentResolution().getReplacementID());
				if (io instanceof LinkedObject)
					newLink.setParent((LinkedObject) io);
			}
			if (resolution.getTypeResolution() != null) {
				IdentifiedObject io = session.getObject(resolution
						.getTypeResolution().getReplacementID());
				if (io instanceof OBOProperty)
					newLink.setType((OBOProperty) io);
			}
			HistoryItem addItem = new CreateLinkHistoryItem(newLink);
			historyItems.add(delItem);
			historyItems.add(addItem);
			if (applyImmediately) {
				LinkedObject child = resolution.getLink().getChild();
				child.removeParent(resolution.getLink());
				child.addParent(newLink);
			}
		}
		return historyItems;
	}

	public static IDWarning getWarning(String id, boolean isDangling,
			OBOSession session,
			Map<String, Collection<IdentifiedObject>> multiIDMap) {

		IdentifiedObject obj = session.getObject(id);
		if (TermUtil.isObsolete(obj)) {
			Collection<IDResolution> resolutions = new LinkedList<IDResolution>();
			for (ObsoletableObject oo : ((ObsoletableObject) obj)
					.getConsiderReplacements()) {
				boolean isReplacementDangling = session.getObject(oo.getID()) == null;
				IDWarning rw = getWarning(oo.getID(), isReplacementDangling,
						session, multiIDMap);
				if (rw == null) {
					IDResolution resolution = new DefaultIDResolution(id, oo
							.getID(), true);
					resolutions.add(resolution);
				} else {
					for (IDResolution r : rw.getResolutions()) {
						IDResolution resolution = new DefaultIDResolution(id, r
								.getReplacementID(), true);
						resolutions.add(resolution);
					}
				}

			}
			for (ObsoletableObject oo : ((ObsoletableObject) obj)
					.getReplacedBy()) {
				boolean isReplacementDangling = session.getObject(oo.getID()) == null;
				IDWarning rw = getWarning(oo.getID(), isReplacementDangling,
						session, multiIDMap);
				if (rw == null) {
					IDResolution resolution = new DefaultIDResolution(id, oo
							.getID(), false);
					resolutions.add(resolution);
				} else {
					for (IDResolution r : rw.getResolutions()) {
						IDResolution resolution = new DefaultIDResolution(id, r
								.getReplacementID(), r
								.requiresUserIntervention());
						resolutions.add(resolution);
					}
				}
			}
			IDWarning warning = new DefaultIDWarning(id, resolutions,
					WarningType.OBSOLETE_ID);
			return warning;
		} else if (multiIDMap.containsKey(id)) {
			Collection<? extends IdentifiedObject> terms = multiIDMap.get(id);
			Collection<IDResolution> resolutions = new LinkedList<IDResolution>();
			for (IdentifiedObject io : terms) {
				boolean isReplacementDangling = session.getObject(io.getID()) == null;
				IDWarning rw = getWarning(io.getID(), isReplacementDangling,
						session, multiIDMap);
				if (rw == null) {
					IDResolution resolution = new DefaultIDResolution(id, io
							.getID(), terms.size() > 1);
					resolutions.add(resolution);
				} else {
					for (IDResolution r : rw.getResolutions()) {
						IDResolution resolution = new DefaultIDResolution(id, r
								.getReplacementID(), terms.size() > 1
								|| r.requiresUserIntervention());
						resolutions.add(resolution);
					}
				}
			}
			IDWarning warning = new DefaultIDWarning(id, resolutions,
					WarningType.SECONDARY_ID);
			return warning;
		} else if (TermUtil.isDangling(obj)) {
			IDResolution resolution = new DefaultIDResolution(id, id, false);
			IDWarning warning = new DefaultIDWarning(id, Collections
					.singleton(resolution), WarningType.DANGLING_ID);
			return warning;
		}
		return null;
	}

	protected static LinkIDWarning getWarning(Link link, OBOSession session,
			MultiMap<String, IdentifiedObject> multiIDMap) {
		IDWarning typeWarning = getWarning(link.getType().getID(), TermUtil
				.isDangling(link.getType()), session, multiIDMap);
		IDWarning parentWarning = getWarning(link.getParent().getID(), TermUtil
				.isDangling(link.getParent()), session, multiIDMap);
		if (typeWarning == null && parentWarning == null)
			return null;
		else
			return new DefaultLinkIDWarning(link, parentWarning, typeWarning);
	}
}
