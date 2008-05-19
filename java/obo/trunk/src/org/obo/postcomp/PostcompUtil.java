package org.obo.postcomp;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.DanglingObjectImpl;
import org.obo.datamodel.impl.DanglingPropertyImpl;
import org.obo.history.CompletesHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.OperationModel;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class PostcompUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PostcompUtil.class);

	/**
	 * Used to sort intersection links when building postcomp ids or names
	 */
	protected static final Comparator<Link> intersectionComparator = new Comparator<Link>() {
		public int compare(Link o1, Link o2) {
			if (!o1.getType().equals(o2.getType())) {
				if (o1.getType().equals(OBOProperty.IS_A))
					return -1;
				if (o2.getType().equals(OBOProperty.IS_A))
					return 1;
				return o1.getType().getID().compareToIgnoreCase(
						o2.getType().getID());
			} else
				return o1.getParent().getID().compareToIgnoreCase(
						o2.getParent().getID());
		}
	};

	/**
	 * Used to sort parsed id results
	 */
	protected static Comparator<String[]> defReqComparator = new Comparator<String[]>() {
		public int compare(String[] a, String[] b) {
			int compVal = a[0].compareToIgnoreCase(b[0]);
			if (compVal != 0) {
				if (a[0].equals(OBOProperty.IS_A.getID()))
					return -1;
				else if (a[1].equals(OBOProperty.IS_A.getID()))
					return 1;
				else
					return compVal;
			} else
				return a[1].compareToIgnoreCase(b[1]);
		}
	};

	protected static TypeNameProvider defaultProvider;

	/**
	 * Equivalent to calling
	 * {@link #createPostcompObject(OBOSession, String, boolean) PostcompUtil.createPostcompObject(session, exp, null, false)}.
	 * 
	 * @param session
	 *            the current ontology session to which the new term(s) will be
	 *            added
	 * @param exp
	 *            a post-composition expression
	 * @return the new term that matches the post-composition expression
	 * @throws ParseException
	 */
	public static OBOClass createPostcompObject(OBOSession session, String exp)
			throws ParseException {
		return createPostcompObject(session, exp, null, false);
	}

	/**
	 * Creates a post-composed term from a post-composition expression of the
	 * form described by the OBO 1.3 Post-composition Extension. The
	 * post-composed term (and any required subterms) will be created
	 * immediately and added to the given ontology.
	 * 
	 * A post-composed term is always an anonymous intersection term. Because
	 * post-composed terms have no normal parent or child relationships,
	 * ontologies that use post-composed terms are best viewed with the reasoner
	 * enabled.
	 * 
	 * Often, several terms must be created to fully resolve a post-composition
	 * expression. For example, to resolve the post-composition expression
	 * "A^part_of(B^develops_from(C))", two terms must be created:
	 * "B^develops_from(C)" and "A^part_of(B^develops_from(C))". All the
	 * supporting terms will be automatically created, but this method only
	 * returns the single outermost term given by the post-composition
	 * expression.
	 * 
	 * This method will make an effort to create as few terms as possible. For
	 * example, if the post-composition term "A^part_of(B^develops_from(C))"
	 * already exists, and the new term "X^part_of(B^develops_from(C))" is
	 * created, the method will not re-create the "B^develops_from(C)" sub-term.
	 * Instead, the method will reuse the existing sub-term. However, there are
	 * often multiple ways to decompose a post-composition expression
	 * (particularly if parenthesis are not used when chaining together many
	 * differentia). If there are multiple possible decompositions for a
	 * post-composition expression, the method will only check to see if the most
	 * obvious parse of the expression has any existing sub-parts. The method will
	 * not attempt the (computationally expensive) task of trying every possible
	 * parse.
	 * 
	 * Normally, this method will throw a {@link ParseException} if the
	 * post-composition expression refers to an identifier that does not exist
	 * in the given {@link OBOSession session}. If the allowDangling parameter
	 * is true, identifiers for non-existant terms may appear in the post-comp
	 * expression. These identifiers will be linked as {@link DanglingObject}s.
	 * Note that dangling type identifiers are not allowed in post-composition
	 * expressions.
	 * 
	 * A TypeNameProvider may be passed to this method, if desired, to improve
	 * the syntax of auto-generated names. If the {@link TypeNameProvider} is
	 * null, a {@link DefaultTypeNameProvider} will be used.
	 * 
	 * @param session
	 *            The session where newly created terms will be added and
	 *            against which identifiers in the post-comp expression will be
	 *            checked
	 * @param exp
	 *            The post-composition expression to resolve
	 * @param provider
	 *            the TypeNameProvider to use for name generation - null is
	 *            legal
	 * @param allowDangling
	 *            Whether or not dangling identifiers are allowed in the
	 *            post-comp expression
	 * @return The post-composed term defined by this expression
	 * @throws ParseException
	 *             if the post-composition expression is malformed or improperly
	 *             references a dangling identifier
	 */
	public static OBOClass createPostcompObject(OBOSession session, String exp,
			TypeNameProvider provider, boolean allowDangling)
			throws ParseException {
		OperationModel model = session.getOperationModel();
		model.setSession(session);
		HistoryItem item = createPostcompItem(session, exp, provider,
				allowDangling);
		model.apply(item);
		OBOClass out = (OBOClass) session.getObject(item.getTarget());
		return out;
	}

	/**
	 * Equivalent to calling
	 * {@link #createPostcompObject(OBOSession, String, TypeNameProvider, boolean) PostcompUtil.createPostcompObject(session, exp, null, allowDangling)}.
	 * 
	 * @param session
	 *            the current ontology session to which the new term(s) will be
	 *            added
	 * @param exp
	 *            a post-composition expression
	 * @param allowDangling
	 *            Whether or not dangling identifiers are allowed in the
	 *            post-comp expression
	 * @return the new term that matches the post-composition expression
	 * @throws ParseException
	 */
	public static OBOClass createPostcompObject(OBOSession session, String exp,
			boolean allowDangling) throws ParseException {
		return createPostcompObject(session, exp, null, allowDangling);
	}

	/**
	 * Equivalent to calling
	 * {@link #createPostcompObject(OBOSession, String, TypeNameProvider, boolean) createPostcompObject(session, exp, provider, boolean)},
	 * except that this method does not create any new terms or modify the
	 * session. Instead, this method returns a {@link HistoryItem} that
	 * describes how to create the new object. The new object will be created
	 * when that {@link HistoryItem} is applied to an {@link OperationModel}.
	 * 
	 * @param session
	 *            The session against which identifiers in the post-comp
	 *            expression will be resolved
	 * @param exp
	 *            The post-composition expression to resolve
	 * @param provider
	 *            the TypeNameProvider to use for name generation - null is
	 *            legal
	 * @param allowDangling
	 *            Whether or not dangling identifiers are allowed in the
	 *            post-comp expression
	 * @return A {@link HistoryItem} that can be used to create the term
	 *         specified by the post-composition expression
	 * @throws ParseException
	 *             if the post-composition expression is malformed or improperly
	 *             references a dangling identifier
	 */
	public static HistoryItem createPostcompItem(OBOSession session,
			String exp, TypeNameProvider provider, boolean allowDangling)
			throws ParseException {
		return createPostcompItem(session, exp,
				new HashMap<OBOClass, Collection<Link>>(), provider,
				allowDangling);
	}

	/**
	 * Equivalent to calling
	 * {@link #createPostcompObject(OBOSession, String, null, boolean)}
	 */
	public static HistoryItem createPostcompItem(OBOSession session,
			String exp, boolean allowDangling) throws ParseException {
		return createPostcompItem(session, exp,
				new HashMap<OBOClass, Collection<Link>>(), null, allowDangling);
	}

	/**
	 * Equivalent to {@link #createPostcompItem(OBOSession, String, boolean)},
	 * except that this version of the method allows the user to pass in a
	 * {@link Map} that will be used by the method to store an internal table of
	 * intersection definitions. If many post-composed terms will be created in
	 * a row, it is more efficient to call this version of the method, because
	 * there will be fewer object creations and redundant work.
	 * 
	 * @param session
	 *            The session against which identifiers in the post-comp
	 *            expression will be resolved
	 * @param exp
	 *            The post-composition expression to resolve
	 * @param intersectionMap
	 *            A Map to use for internal bookkeeping
	 * @param allowDangling
	 *            Whether or not dangling identifiers are allowed in the
	 *            post-comp expression
	 * @return A {@link HistoryItem} that can be used to create the term
	 *         specified by the post-composition expression
	 * @throws ParseException
	 *             if the post-composition expression is malformed or improperly
	 *             references a dangling identifier
	 */
	public static HistoryItem createPostcompItem(OBOSession session,
			String exp, Map<OBOClass, Collection<Link>> intersectionMap,
			TypeNameProvider provider, boolean allowDangling)
			throws ParseException {
		OBOPostcomp t = new OBOPostcomp(new StringReader(exp));
		ASTPostcompExpression expression = (ASTPostcompExpression) t
				.PostcompExpression();
		TermMacroHistoryItem item = new TermMacroHistoryItem();
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof OBOClass) {
				OBOClass oboClass = (OBOClass) io;
				Collection<Link> intersections = null;

				for (Link link : oboClass.getParents()) {
					if (TermUtil.isIntersection(link)) {
						if (intersections == null) {
							intersections = new LinkedList<Link>();
							intersectionMap.put(oboClass, intersections);
						}
						intersections.add(link);
					}
				}
			}
		}

		String id = createPostcompItem(item, intersectionMap, session,
				(ASTCompoundExpression) expression.jjtGetChild(0), provider,
				allowDangling);
		item.setTarget(id);
		return item;
	}

	/**
	 * An internal recursive method used to resolve the postcomp definition.
	 * 
	 * @param item
	 * @param intersectionMap
	 * @param session
	 * @param node
	 * @param allowDangling
	 * @return
	 * @throws ParseException
	 */
	protected static String createPostcompItem(TermMacroHistoryItem item,
			Map<OBOClass, Collection<Link>> intersectionMap,
			OBOSession session, ASTCompoundExpression node,
			TypeNameProvider provider, boolean allowDangling)
			throws ParseException {
		List<String[]> defReqs = new LinkedList<String[]>();
		for (Node n : node.children) {
			if (n instanceof ASTGenusExpression) {
				String[] pair = { OBOProperty.IS_A.getID(),
						getIdentifier(((ASTGenusExpression) n)) };
				defReqs.add(pair);
			} else if (n instanceof ASTDifferentiaExpression) {
				String typeID = getTypeIdentifier((ASTDifferentiaExpression) n);
				ASTCompoundExpression target = getDiffTarget((ASTDifferentiaExpression) n);
				String targetID = createPostcompItem(item, intersectionMap,
						session, target, provider, allowDangling);
				String[] pair = { typeID, targetID };
				defReqs.add(pair);
			} else if (n instanceof ASTCompoundExpression) {
				String id = createPostcompItem(item, intersectionMap, session,
						(ASTCompoundExpression) n, provider, allowDangling);
				String[] pair = { OBOProperty.IS_A.getID(), id };
				defReqs.add(pair);
			}
		}
		if (defReqs.size() == 1
				&& defReqs.get(0)[0].equals(OBOProperty.IS_A.getID())) {
			return defReqs.get(0)[1];
		} else {
			Collections.sort(defReqs, defReqComparator);
			OBOClass classMatch = null;
			for (OBOClass oboClass : intersectionMap.keySet()) {
				Collection<Link> links = intersectionMap.get(oboClass);
				Collection<String[]> defReqsCopy = new LinkedList<String[]>(
						defReqs);
				boolean foundAll = true;
				for (Link link : links) {
					String[] match = null;
					for (String[] pair : defReqsCopy) {
						if (link.getType().getID().equals(pair[0])
								&& link.getParent().getID().equals(pair[1])) {
							match = pair;
							break;
						}
					}
					if (match == null) {
						foundAll = false;
						break;
					} else {
						defReqsCopy.remove(match);
					}
				}
				if (foundAll && defReqsCopy.size() == 0) {
					classMatch = oboClass;
					break;
				}
			}
			if (classMatch != null)
				return classMatch.getID();
			else {
				StringBuffer buffer = new StringBuffer();
				boolean first = true;
				for (String[] temp : defReqs) {
					if (!first)
						buffer.append("^");
					if (temp[0].equals(OBOProperty.IS_A.getID())) {
						buffer.append(temp[1]);
					} else {
						buffer.append(temp[0]);
						buffer.append('(');
						buffer.append(temp[1]);
						buffer.append(')');
					}
					first = false;
				}
				String newID = buffer.toString();
				item.addItem(new CreateObjectHistoryItem(newID, true,
						OBOClass.OBO_CLASS.getID()));
				OBOClass oboClass = (OBOClass) session.getObjectFactory()
						.createObject(newID, OBOClass.OBO_CLASS, true);
				Collection<Link> links = new LinkedList<Link>();
				for (String[] temp : defReqs) {
					LinkedObject parent = getObject(temp[1], session,
							intersectionMap);
					if (parent == null) {
						if (allowDangling)
							parent = new DanglingObjectImpl(temp[1]);
						else
							throw new ParseException("Cannot resolve term id "
									+ temp[1]);
					}
					OBOProperty type = (OBOProperty) session.getObject(temp[0]);
					if (type == null) {
						if (allowDangling)
							type = new DanglingPropertyImpl(temp[0]);
						else
							throw new ParseException("Cannot resolve type id "
									+ temp[0]);
					}
					OBORestriction link = session
							.getObjectFactory()
							.createOBORestriction(oboClass, type, parent, false);
					link.setCompletes(true);
					item.addItem(new CreateLinkHistoryItem(oboClass.getID(),
							temp[0], temp[1]));
					item.addItem(new CompletesHistoryItem(oboClass.getID(),
							temp[0], temp[1], false));
					links.add(link);
					oboClass.atomicAddParent(link);
				}
				intersectionMap.put(oboClass, links);
				String name = getPostcompName(oboClass, provider);
				oboClass.setName(name);
				item.addItem(new NameChangeHistoryItem(name, newID,
						newID));
				return newID;
			}
		}
	}

	/**
	 * An internal method used to resolve an identifier into a term. This method
	 * checks the current session for an id match. If one isn't found, the
	 * collection of newly created terms is checked.
	 * 
	 * @param id
	 * @param session
	 * @param map
	 * @return
	 */
	protected static OBOClass getObject(String id, OBOSession session,
			Map<OBOClass, Collection<Link>> map) {
		OBOClass oboClass = (OBOClass) session.getObject(id);
		if (oboClass != null)
			return oboClass;
		for (OBOClass temp : map.keySet()) {
			if (temp.getID().equals(id))
				return temp;
		}
		return null;
	}

	protected static String getIdentifier(ASTGenusExpression node) {
		return ((ASTIdentifier) node.jjtGetChild(0)).val;
	}

	protected static String getTypeIdentifier(ASTDifferentiaExpression node) {
		return ((ASTIdentifier) node.jjtGetChild(0)).val;
	}

	protected static ASTCompoundExpression getDiffTarget(
			ASTDifferentiaExpression node) {
		return (ASTCompoundExpression) node.jjtGetChild(1);
	}

	/**
	 * Equivalent to
	 * {@link #getPostcompID(IdentifiedObject, boolean) getPostcompID(io, false)}
	 * 
	 * @param io
	 * @return
	 */
	public static String getPostcompID(IdentifiedObject io) {
		return getPostcompID(io, false);
	}

	/**
	 * Gets a post-composition expression for the given term. If the term is not
	 * an anonymous intersection, this method's result is the same as the term's
	 * {@link IdentifiedObject#getID() getID()} method. If the term is an
	 * anonymous intersection, this method returns a post-composition expression
	 * for the term.
	 * 
	 * Normally this method will not decompose non-anonymous intersections. If
	 * alwaysDecompose is true, all intersections will be decomposed.
	 * 
	 * @param io
	 *            the term for which to find a post-composition expression
	 * @param alwaysDecompose
	 *            whether to decompose non-anonymous intersections
	 * @return a valid post-composition expression
	 */
	public static String getPostcompID(IdentifiedObject io,
			boolean alwaysDecompose) {
		if (PostcompUtil.isPostcompable(io, alwaysDecompose)) {
			OBOClass oboClass = (OBOClass) io;
			List<Link> links = new ArrayList<Link>();
			for(Link link : oboClass.getParents()) {
				if (TermUtil.isIntersection(link))
					links.add(link);
			}
			Collections.sort(links, PostcompUtil.intersectionComparator);
			StringBuffer buffer = new StringBuffer();
			boolean first = true;
			for (Link link : links) {
				if (!first)
					buffer.append('^');
				if (link.getType().equals(OBOProperty.IS_A)) {
					if (PostcompUtil.isPostcompable(link.getParent(),
							alwaysDecompose)) {
						buffer.append('(');
						buffer.append(getPostcompID(link.getParent(),
								alwaysDecompose));
						buffer.append(')');
					} else
						buffer.append(link.getParent().getID());
				} else {
					buffer.append(link.getType().getID());
					buffer.append('(');
					buffer.append(getPostcompID(link.getParent(),
							alwaysDecompose));
					buffer.append(')');
				}
				first = false;
			}
			return buffer.toString();
		} else
			return io.getID();
	}

	/**
	 * Gets a post-composition expression for the given term, but using term
	 * names instead of ids. The result of this method is NOT a valid postcomp
	 * expression and cannot be parsed by these tools, but it can be useful for
	 * user displays.
	 * 
	 * @param io
	 *            the term for which to find a post-composition name expression
	 * @param alwaysDecompose
	 *            whether to decompose non-anonymous intersections
	 * @return a post-composition name expression
	 */
	public static String getNameExpression(IdentifiedObject io,
			boolean alwaysDecompose) {
		if (PostcompUtil.isPostcompable(io, alwaysDecompose)) {
			OBOClass oboClass = (OBOClass) io;
			List<Link> links = new ArrayList<Link>();
			for(Link link : oboClass.getParents()) {
				if (TermUtil.isIntersection(link))
					links.add(link);
			}
			Collections.sort(links, PostcompUtil.intersectionComparator);
			StringBuffer buffer = new StringBuffer();
			boolean first = true;
			for (Link link : links) {
				if (!first)
					buffer.append('^');
				updateNameExpBuffer(buffer, link, alwaysDecompose);
				first = false;
			}
			return buffer.toString();
		} else
			return io.getName();
	}

	public static String getNameExpression(Collection<Link> linksIn,
			boolean alwaysDecompose) {
		List<Link> links = new ArrayList<Link>();
		links.addAll(linksIn);
		Collections.sort(links, PostcompUtil.intersectionComparator);
		StringBuffer buffer = new StringBuffer();
		boolean first = true;
		for (Link link : links) {
			if (!first)
				buffer.append('^');
			updateNameExpBuffer(buffer, link, alwaysDecompose);
			first = false;
		}
		return buffer.toString();

	}

	protected static void updateNameExpBuffer(StringBuffer buffer, Link link,
			boolean alwaysDecompose) {
		if (link.getType().equals(OBOProperty.IS_A)) {
			if (PostcompUtil.isPostcompable(link.getParent(), alwaysDecompose)) {
				buffer.append('(');
				buffer.append(getNameExpression(link.getParent(),
						alwaysDecompose));
				buffer.append(')');
			} else
				buffer.append(link.getParent().getName());
		} else {
			buffer.append(link.getType().getID());
			buffer.append('(');
			buffer.append(getNameExpression(link.getParent(), alwaysDecompose));
			buffer.append(')');
		}

	}

	/**
	 * Internal method used to create and cache a default
	 * {@link TypeNameProvider} if necessary.
	 * 
	 * @return
	 */
	protected static TypeNameProvider getDefaultProvider() {
		if (defaultProvider == null)
			defaultProvider = new DefaultTypeNameProvider();
		return defaultProvider;
	}

	/**
	 * Equivalent to
	 * {@link #getPostcompName(IdentifiedObject, TypeNameProvider, boolean) getPostCompName(io, provider, false)}
	 * 
	 * @param io
	 * @param provider
	 * @return
	 */
	public static String getPostcompName(IdentifiedObject io,
			TypeNameProvider provider) {
		return getPostcompName(io, provider, false);
	}

	/**
	 * Gets the automatically generated name for a post-composed term. If the
	 * term is not an anonymous intersection, this method's result is the same
	 * as the term's {@link IdentifiedObject#getName() getName()} method. If the
	 * term is an anonymous intersection, this method returns a simple readable
	 * name for this term.
	 * 
	 * Normally this method will not decompose a non-anonymous intersection,
	 * because non-anonymous terms usually have good, descriptive names.
	 * However, if the alwaysDecompose argument is true, this method will
	 * decompose every intersection name.
	 * 
	 * @param io
	 *            the term for which to find a post-composition expression
	 * @param provider
	 *            used to map type names to strings that fit into the grammar
	 *            output by this method
	 * @param alwaysDecompose
	 *            whether to always full parse a postcomp name, even if the
	 *            object has a
	 * @return a valid post-composition expression
	 */
	public static String getPostcompName(IdentifiedObject io,
			TypeNameProvider provider, boolean alwaysDecompose) {
		if (PostcompUtil.isPostcompable(io, alwaysDecompose)) {
			if (provider == null)
				provider = getDefaultProvider();
			OBOClass oboClass = (OBOClass) io;
			List<Link> links = new ArrayList<Link>();
			for(Link link : oboClass.getParents()) {
				if (TermUtil.isIntersection(link))
					links.add(link);
			}
			
			// sometimes we create post-comps without having the full ontology
			// loaded - in these cases we can't create the name
			boolean nameable = false;
			for (Link link : links) {
				if (link.getParent().getName() != null) {
					nameable = true;
					break;
				}
			}
			if (!nameable)
				return null;
			
			Collections.sort(links, PostcompUtil.intersectionComparator);
			StringBuffer buffer = new StringBuffer();
			boolean first = true;
			for (Link link : links) {
				if (!first)
					buffer.append(" that ");
				updatePostcompNameBuffer(buffer, link, alwaysDecompose,
						provider, first);
				first = false;
			}
			return buffer.toString();
		} else
			return io.getName() == null ? io.getName() : io.getID();
	}

	public static String getPostcompName(Collection<Link> linksIn,
			TypeNameProvider provider, boolean alwaysDecompose) {
		if (provider == null)
			provider = getDefaultProvider();
		List<Link> links = new ArrayList<Link>();
		links.addAll(linksIn);
		Collections.sort(links, PostcompUtil.intersectionComparator);
		StringBuffer buffer = new StringBuffer();
		boolean first = true;
		for (Link link : links) {
			if (!first)
				buffer.append(" that ");
			updatePostcompNameBuffer(buffer, link, alwaysDecompose, provider,
					first);
			first = false;
		}
		return buffer.toString();

	}

	protected static void updatePostcompNameBuffer(StringBuffer buffer,
			Link link, boolean alwaysDecompose, TypeNameProvider provider,
			boolean first) {
		if (link.getType().equals(OBOProperty.IS_A)) {
			if (!first)
				buffer.append(" is a ");
			if (PostcompUtil.isPostcompable(link.getParent(), alwaysDecompose)) {
				buffer.append(getPostcompName(link.getParent(), provider,
						alwaysDecompose));
			} else
				buffer.append(link.getParent().getName());
		} else {
			buffer.append(provider.getName(link.getType()));
			buffer.append(' ');
			buffer.append(getPostcompName(link.getParent(), provider,
					alwaysDecompose));
		}
	}

	protected static boolean isPostcompable(IdentifiedObject io,
			boolean alwaysDecompose) {
		if (alwaysDecompose)
			return (io instanceof LinkedObject)
					&& TermUtil.isIntersection((LinkedObject) io);
		else
			return isPostcompable(io);
	}

	public static boolean isPostcompable(IdentifiedObject io) {
		if (io.isAnonymous() && io instanceof OBOClass) {
			OBOClass oboClass = (OBOClass) io;
			int intersectionCount = 0;
			for (Link link : oboClass.getParents()) {
				if (TermUtil.isIntersection(link))
					intersectionCount++;
			}
			if (intersectionCount > 0
					&& intersectionCount == oboClass.getParents().size()) {
				return true;
			}
		}
		return false;
	}

}
