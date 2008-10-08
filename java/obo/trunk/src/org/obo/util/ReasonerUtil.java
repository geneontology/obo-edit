package org.obo.util;


import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;

import org.apache.log4j.*;

public class ReasonerUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerUtil.class);

	public static boolean containsExplanation(ReasonedLinkDatabase reasoner,
			Explanation e) {
		Collection<Explanation> explanations = reasoner.getExplanations(e
				.getExplainedObject());
		return explanations != null && explanations.contains(e);
	}

	/**
	 * @param reasoner
	 * @param link
	 * @return
	 */
	@Deprecated
	public static Collection<PathCapable> getShortestExplanationPath(ReasonedLinkDatabase reasoner,
			PathCapable link) {
		int leastHops = 0;
		Collection<PathCapable> shortestPath = new LinkedList<PathCapable>();
		logger.info("Link: "+link);
		for (Explanation e : reasoner.getExplanations(link)) {
			logger.info("  Explanation: "+e);
			if (e.getExplanationType().equals(ExplanationType.GIVEN))
				return Collections.singleton(link);

			// sum over evidence
			Collection<PathCapable> path = new LinkedList<PathCapable>();
			for (Link evidence : e.getEvidence()) {
				logger.info("    Evidence: "+e);
				path.addAll(getShortestExplanationPath(reasoner,evidence));
			}
			if (leastHops == 0 || path.size() <= leastHops) {
				leastHops = path.size();
				shortestPath = path;
			}
		}

		Collection<PathCapable> newPath = new LinkedList<PathCapable>();
		newPath.add(link);
		newPath.addAll(shortestPath);
		return newPath;
	}

	public static Collection<PathCapable> getShortestExplanationPath(ReasonedLinkDatabase reasoner,
			LinkedObject child, OBOProperty prop, LinkedObject parent) {
		Link link = new OBORestrictionImpl(child,prop,parent);
		return getShortestExplanationPath(reasoner,link);
	}


	public static boolean generateTransitiveImplication(
			ReasonedLinkDatabase reasoner, Link out, Link link, Link gpLink) {

		if (gpLink.getType().isNonInheritable()
				|| link.getType().isNonInheritable())
			return false;
		if (!link.getParent().equals(gpLink.getChild()))
			throw new RuntimeException("link and gpLink don't fit!");
		if (reasoner.isSubPropertyOf(link.getType(), OBOProperty.IS_A)) {
			out.setChild(link.getChild());
			out.setType(gpLink.getType());
			out.setParent(gpLink.getParent());
			return true;
		} else if (reasoner.isSubPropertyOf(gpLink.getType(), OBOProperty.IS_A)) {
			out.setChild(link.getChild());
			out.setType(link.getType());
			out.setParent(gpLink.getParent());
			return true;
		} else if (link.getType().isTransitive()
				&& gpLink.getType().isTransitive()) {
			if (reasoner.isSubPropertyOf(link.getType(), gpLink.getType())) {
				out.setChild(link.getChild());
				out.setType(gpLink.getType());
				out.setParent(gpLink.getParent());
				return true;
			} else if (reasoner.isSubPropertyOf(gpLink.getType(), link
					.getType())) {
				out.setChild(link.getChild());
				out.setType(link.getType());
				out.setParent(gpLink.getParent());
				return true;
			}
		}
		return false;
		/*
		 * 
		 * if (!link.getParent().equals(gpLink.getChild())) throw new
		 * RuntimeException("link and gpLink don't fit!"); if
		 * (TermUtil.isIntersection(link) || TermUtil.isIntersection(gpLink))
		 * throw new IllegalArgumentException( "Transitivity doesn't work over
		 * intersections"); if (reasoner.isSubPropertyOf(link.getType(),
		 * OBOProperty.IS_A)) { out.setChild(link.getChild());
		 * out.setType(gpLink.getType()); out.setParent(gpLink.getParent());
		 * return true; } else if (reasoner.isSubPropertyOf(gpLink.getType(),
		 * OBOProperty.IS_A)) { out.setChild(link.getChild());
		 * out.setType(link.getType()); out.setParent(gpLink.getParent());
		 * return true; } else if (link.getType().isTransitive() &&
		 * gpLink.getType().isTransitive()) { if
		 * (reasoner.isSubPropertyOf(link.getType(), gpLink.getType())) {
		 * out.setChild(link.getChild()); out.setType(gpLink.getType());
		 * out.setParent(gpLink.getParent()); return true; } else if
		 * (reasoner.isSubPropertyOf(gpLink.getType(), link .getType())) {
		 * out.setChild(link.getChild()); out.setType(link.getType());
		 * out.setParent(gpLink.getParent()); return true; } } return false;
		 */
	}

	protected static boolean onlyHasGenusDiffExplanation(
			ReasonedLinkDatabase linkDatabase, Link link) {
		if (!(linkDatabase instanceof ReasonedLinkDatabase))
			return false;
		Collection<Explanation> exps = ((ReasonedLinkDatabase) linkDatabase)
		.getExplanations(link);
		boolean foundGenusDiff = false;
		for (Explanation e : exps) {
			if (!e.getExplanationType().equals(ExplanationType.GENUS)
					&& !e.getExplanationType().equals(
							ExplanationType.DIFFERENTIA)) {
				return false;
			}
		}
		return exps.size() > 0;
	}

	/**
	 * @see shouldBeTrimmedOld
	 * @param linkDatabase
	 * @param inLink
	 * @return
	 */
	public static boolean shouldBeTrimmedNew(LinkDatabase linkDatabase, Link inLink) {
		if (!TermUtil.isImplied(inLink))
			return false;
		if (TermUtil.isIntersection(inLink))
			return true;
		// if (onlyHasGenusDiffExplanation(reasoner, inLink))
		// return false;
		Collection<Link> parents = new LinkedList<Link>(linkDatabase
				.getParents(inLink.getChild()));
		Collection<Link> children = linkDatabase
		.getChildren(inLink.getParent());
		OBORestriction scratch = new OBORestrictionImpl();
		for (Link link : parents) {
			if (TermUtil.isIntersection(link))
				continue;
			scratch.setChild(link.getParent());
			scratch.setType(inLink.getType());
			scratch.setParent(inLink.getParent());
			// logger.info("parents = " + parents);
			// logger.info("children = " + children);
			if (children.contains(scratch)) {
				// logger.info("explanations: "
				// + linkDatabase.getExplanations(inLink));
				return true;
			}

			scratch.setType(OBOProperty.IS_A);
			if (children.contains(scratch))
				return true;
		}
		return false;
	}

	/**
	 * false if the link is not implied; true if implied and an intersection link (does this happen?).
	 * true if this link is the same as a grandparent link
	 * 
	 * Note: despite its name this appears to be the current means of trimming in
	 * TrimmedLinkDatabase
	 * @param linkDatabase
	 * @param link
	 * @return
	 */
	public static boolean shouldBeTrimmed(LinkDatabase linkDatabase,
			Link link) {
		if (!TermUtil.isImplied(link))
			return false; // asserted links are never trimmed
		if (TermUtil.isIntersection(link))
			return true; // TODO
		if (!link.getType().equals(OBOProperty.IS_A))
			return true; // trim all implied non-is_a links
		if (link.getChild().equals(link.getParent()))
			return true; // trim all reflexive links

		// trim any links to parents that are redundant with
		// a GRANDPARENT link
		// if any trimmed link is a GIVEN link, it is redundant


		// links that originate from the same child
		// link = <A R B>, parent = <A R2 X>
		Collection<Link> parents = linkDatabase.getParents(link.getChild());

		// for each parent link
		for (Link parentLink : parents) {

			if (parentLink.equals(link)) {
				continue;
			}
			if (parentLink.getChild().equals(parentLink.getParent())) {
				continue; // reflexive
			}

			if (!(parentLink.getType().equals(link.getType()) || parentLink
					.getType().equals(OBOProperty.IS_A)))
				continue;

			// relations are identical; R2=R
			boolean sawType = parentLink.getType().equals(link.getType());


			// for each grandparent link accessible via the current
			// parent link...
			// <X R3 B2>
			for (Link gpLink : linkDatabase.getParents(parentLink.getParent())) {
				if (gpLink.getChild().equals(gpLink.getParent())) {
					continue; // reflexive
				}
				if (TermUtil.isIntersection(gpLink))
					continue;
				
				// see if the grandparent link has the same type
				// and parent as the current link. if it does,
				// the current link is redundant with the grandparent
				// link and should be removed
				if (link.getParent().equals(gpLink.getParent())) { // B=B2
					if ((!sawType || link.getType().isTransitive()) &&
							link.getType().equals(gpLink.getType())) {
						return true; 
					}
					if (sawType && gpLink.getType().equals(OBOProperty.IS_A)) {
						return true;
					}
				}
			}

			// add a section where we trim links that have a sibling link
			// with the same parent, but a more specific type,
			// than the current link
		}

		return false;

	}

	public static Collection<Link> getGivenSupportingLinks(
			ReasonedLinkDatabase database, Link link) {
		Collection<Link> out = new LinkedList<Link>();
		ReasonerUtil.populateGivenSupportingLinks(out, database, link);
		out.remove(link);
		return out;
	}

	public static Collection<PathCapable> getImmediateSupportingLinks(
			ReasonedLinkDatabase database, Link link) {
		Collection<PathCapable> out = new LinkedList<PathCapable>();

		Collection<Explanation> exps = database.getExplanations(link);
		for (Explanation explanation : exps) {
			for (Link evLink : explanation.getEvidence()) {
				out.add(evLink);
			}
		}
		return out;
	}

	public static List<LinkedObject> getMostSpecific(
			ReasonedLinkDatabase reasoner, Collection<LinkedObject> objects,
			OBOProperty property) {
		List<LinkedObject> out = new ArrayList<LinkedObject>();
		List<LinkedObject> in = new ArrayList<LinkedObject>(objects);
		for (int i = 0; i < in.size(); i++) {
			LinkedObject a = in.get(i);
			boolean found = false;
			for (int j = i + 1; j < in.size(); j++) {
				LinkedObject b = in.get(j);
				if (reasoner.hasRelationship(b, property, a) != null) {
					found = true;
					break;
				}
			}
			if (!found) {
				out.add(a);
			}
		}
		return out;
	}

	public static boolean isDisjoint(ReasonedLinkDatabase linkDatabase,
			OBOClass a, OBOClass b, boolean siblingsAreDisjoint) {
		// classes are never disjoint with themselves
		if (a.equals(b))
			return false;

		if (siblingsAreDisjoint) {
			// classes are never disjoint with their super/subclasses
			if (linkDatabase.isSubclassOf(a, b)
					|| linkDatabase.isSubclassOf(b, a))
				return false;

			// if the two classes have ANY superclasses in common, but
			// aren't sub or superclasses of each other, they
			// are disjoint
			Collection<LinkedObject> a_superClasses = linkDatabase
			.getParentsOfType(a, OBOProperty.IS_A);
			Collection<LinkedObject> b_superClasses = linkDatabase
			.getParentsOfType(b, OBOProperty.IS_A);
			a_superClasses.retainAll(b_superClasses);
			return a_superClasses.size() > 0;
		} else {
			Collection<LinkedObject> disjoints = linkDatabase.getParentsOfType(
					a, OBOProperty.DISJOINT_FROM);
			if (disjoints.contains(b))
				return true;

			// remove this section once the reasoner starts handling
			// symmetry properly
			disjoints = linkDatabase.getParentsOfType(b,
					OBOProperty.DISJOINT_FROM);
			return disjoints.contains(a);
		}
	}

	//@Deprecated
	public static boolean isExplanationForLinkCyclic(ReasonedLinkDatabase reasoner, Explanation exp, Link link) {
		return isExplanationForLinkCyclic(reasoner, exp, link, new HashSet<Link>());
	}

	/**
	 * explanations are recursive. Some explanation chains m ay be cyclic. For example, an asserted link can end up supporting itself in an intersection rule.
	 * 
	 * this method is currently deprecated: to correctly check for this we may have to either
	 * (1) remove the link, then check if the link can still be inferred after the cascading delete
	 * or (2) build all possible explanation chains and make sure  
	 * @param reasoner
	 * @param exp
	 * @param link
	 * @param checkedLinks
	 * @return true if the chain of explanations includes link as evidence
	 */
	//@Deprecated
	public static boolean isExplanationForLinkCyclic(ReasonedLinkDatabase reasoner, Explanation exp, Link link, Set<Link> checkedLinks) {
		//Set<Link> checkedLinks = new HashSet<Link>(oldCheckedLinks.size());
		//for (Link cl : oldCheckedLinks)
		//	checkedLinks.add(cl);
		//System.out.println(checkedLinks);
		//System.out.println("checking: "+link);
		for (Link evidenceLink : exp.getEvidence()) {
			if (checkedLinks.contains(evidenceLink)) {
				//continue;
				return true;
			}
			checkedLinks.add(evidenceLink);

			if (evidenceLink.equals(link)) {
				return true;
			}
			boolean hasNonCyclicExplanation = false;
			// any one explanation is sufficient
			for (Explanation evidenceExp : reasoner.getExplanations(evidenceLink)) {
				if (isExplanationForLinkCyclic(reasoner, evidenceExp, link, checkedLinks)) {
					//System.out.println("circular evidence for: "+link+" evLink="+evidenceLink+" exp="+evidenceExp+" IN: "+exp);
					//return true;
				}
				else {
					hasNonCyclicExplanation = true;
				}
			}
			if (!hasNonCyclicExplanation) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 * @param reasoner
	 * @param link
	 * @param isRepairMode
	 * @return explanation for why link is redundant. Returns null if link non-redundant. If multiple explanations, return first
	 */
	public static Explanation getRedundancyExplanation(ReasonedLinkDatabase reasoner, Link link, Boolean isRepairMode) {
		if (TermUtil.isIntersection(link))
			return null; // N+S conditions are never false
		if (TermUtil.isImplied(link))
			return null; // only asserted links can be redundant
		for (Explanation exp : reasoner.getExplanations(link)) {
			if (exp.getExplanationType().equals(ExplanationType.GIVEN)) {
				continue;
			}
			if (isRepairMode) {
				// repair mode: we have redundant is_a links that can be inferred by a reasoner. in repair mode we do not treat these as redundant
				if (exp.getExplanationType().equals(ExplanationType.GENUS))
					continue;
				if (exp.getExplanationType().equals(ExplanationType.DIFFERENTIA))
					continue;
				if (exp.getExplanationType().equals(ExplanationType.INTERSECTION))
					continue;
			}
			// from here on we are not in repair mode; i.e. if a link can be inferred by an xp rule then it is considered redundant

			if (exp.getExplanationType().equals(ExplanationType.INTERSECTION)) {
				continue;
				// test: negative regulation of programmed cell death --OBO_REL:is_a--> negative regulation of developmental process
				// sometimes intersection links can explain themselves; we want to make sure these are not treated as redundant
				//if (isExplanationForLinkCyclic(reasoner, exp,link)) { 
				//	continue;
				//}
			}
			return exp;			
		}
		return null;
	}

	public static boolean isRedundant(ReasonedLinkDatabase reasoner, Link link, Boolean isRepairMode) {
		return getRedundancyExplanation(reasoner, link, isRepairMode) != null;
	}

	public static boolean isRedundant(ReasonedLinkDatabase reasoner, Link link) {
		return isRedundant(reasoner,link,true);
	}

	@Deprecated
	public static boolean isRedundantDEPRECATED(ReasonedLinkDatabase reasoner, Link link) {
		if (TermUtil.isIntersection(link))
			return false; // N+S conditions are never false
		Collection<Explanation> exps = reasoner.getExplanations(link);

		// why was this here? --CJM
		//if (exps.size() == 1)
		//	return false;

		Iterator<Explanation> it2 = exps.iterator();
		boolean hasGiven = false;
		boolean nonGenus = false;
		for (int i = 0; it2.hasNext(); i++) {
			Explanation exp = it2.next();
			if (exp.getExplanationType().equals(ExplanationType.GIVEN)) {
				hasGiven = true;
			} else if (!exp.getExplanationType().equals(ExplanationType.GENUS))
				nonGenus = true;

			if (hasGiven && nonGenus)
				break;
		}
		return hasGiven && nonGenus;
	}

	public static Collection<Link> getAllRedundantLinks(ReasonedLinkDatabase reasoner, LinkDatabase ldb) {
		Collection<Link> redundantLinks = new LinkedHashSet<Link>();
		Iterator<Link> it = TermUtil.getAllLinks(ldb);

		while (it.hasNext()) {
			Link link = it.next();
			if (isRedundant(reasoner,link,true)) { // TODO: make configurable
				redundantLinks.add(link);
			}
		}
		return redundantLinks;
	}


	public static boolean isSubclass(LinkDatabase linkDatabase, OBOProperty a,
			OBOProperty b) {
		if (linkDatabase instanceof ReasonedLinkDatabase)
			return ((ReasonedLinkDatabase) linkDatabase).isSubPropertyOf(a, b);
		else {
			Iterator it = linkDatabase.getParents(a).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				if (link.getType().equals(OBOProperty.IS_A)
						&& link.getParent().equals(b))
					return true;
			}
			return false;
		}
	}

	public static boolean isSubclass(LinkedObject a, LinkedObject b) {
		if (a.equals(b))
			return true;
		Iterator it = a.getParents().iterator();
		while (it.hasNext()) {
			Link tr = (Link) it.next();
			if (isSubclass(tr.getType(), OBOProperty.IS_A)
					&& isSubclass(tr.getParent(), b))
				return true;
		}
		return false;
	}

	public static boolean isSubclass(LinkDatabase db, LinkedObject a,
			LinkedObject b) {
		if (a.equals(b))
			return true;
		for (Link tr : db.getParents(a)) {
			if (tr.getType().equals(OBOProperty.IS_A)
					&& isSubclass(db, tr.getParent(), b))
				return true;
		}
		return false;
	}

	public static void populateGivenSupportingLinks(Collection<Link> out,
			ReasonedLinkDatabase database, Link link) {
		populateGivenSupportingLinks(out, database, link, new HashSet<Link>());
	}

	public static void populateGivenSupportingLinks(Collection<Link> out,
			ReasonedLinkDatabase database, Link link, Collection<Link> seenIt) {
		if (seenIt.contains(link))
			return;
		seenIt.add(link);
		Collection<Explanation> exps = database.getExplanations(link);
		if (!TermUtil.isImplied(link)) {
			out.add(link);
		}
		for (Explanation explanation : exps) {
			for (Link evLink : explanation.getEvidence()) {
				populateGivenSupportingLinks(out, database, evLink, seenIt);
			}
		}
	}

	public static Collection<OBOClass> getGenae(OBOClass oboClass) {
		Collection<OBOClass> out = new LinkedList<OBOClass>();
		for (Link parentLink : oboClass.getParents()) {
			if (parentLink.getType().equals(OBOProperty.IS_A)
					&& TermUtil.isIntersection(parentLink)) {

				out.add(TermUtil.castToClass(parentLink.getParent()));
			}
		}
		return out;
	}

	/**
	 * @author cjm Returns the genus of a cross-product class Returns null if no
	 *         genus is found Assumes max 1 genus per xp class - throws
	 *         exception otherwise TODO: Make a more informative exception
	 */
	public static OBOClass getGenus(OBOClass obj) {
		OBOClass genus = null;

		for (OBOClass g : getGenae(obj)) {
			if (genus != null) {
				throw new RuntimeException(">1 genus for " + obj);
			}
			genus = g;
		}
		return genus;
	}

	public static Collection<Link> getDifferentia(OBOClass oboClass) {
		Collection<Link> out = new LinkedList<Link>();
		for (Link parentLink : oboClass.getParents()) {
			if (!parentLink.getType().equals(OBOProperty.IS_A)
					&& TermUtil.isIntersection(parentLink))
				out.add(TermUtil.castParentToClass(parentLink));
		}
		return out;

	}

	public static Collection<OBOClass> getDifferentiaByType(OBOClass oboClass,
			OBOProperty prop) {
		Collection<OBOClass> out = new LinkedList<OBOClass>();
		for (Link parentLink : oboClass.getParents()) {
			if (parentLink.getType().equals(prop)
					&& TermUtil.isIntersection(parentLink))
				out.add(TermUtil.castToClass(parentLink.getParent()));
		}
		return out;

	}

	public static Collection<OBOClass> getDifferentiaByType(OBOClass oboClass,
			String propName) {
		Collection<OBOClass> out = new LinkedList<OBOClass>();
		for (Link parentLink : oboClass.getParents()) {
			if (parentLink.getType().getName() != null
					&& parentLink.getType().getName().equals(propName)
					&& TermUtil.isIntersection(parentLink))
				out.add(TermUtil.castToClass(parentLink.getParent()));
		}
		return out;

	}

}
