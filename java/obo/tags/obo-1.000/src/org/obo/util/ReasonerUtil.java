package org.obo.util;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;

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

public class ReasonerUtil {

	public static boolean containsExplanation(ReasonedLinkDatabase reasoner,
			Explanation e) {
		Collection<Explanation> explanations = reasoner.getExplanations(e
				.getExplainedObject());
		return explanations != null && explanations.contains(e);
	}

	public static boolean generateTransitiveImplication(
			ReasonedLinkDatabase reasoner, Link out, Link link,
			Link gpLink) {

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

	public static boolean shouldBeTrimmed(LinkDatabase linkDatabase, Link inLink) {
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
			// System.err.println("parents = " + parents);
			// System.err.println("children = " + children);
			if (children.contains(scratch)) {
				// System.err.println("explanations: "
				// + linkDatabase.getExplanations(inLink));
				return true;
			}

			scratch.setType(OBOProperty.IS_A);
			if (children.contains(scratch))
				return true;
		}
		return false;
	}

	public static boolean shouldBeTrimmedOld(LinkDatabase linkDatabase,
			Link link) {
		if (!TermUtil.isImplied(link))
			return false;
		Iterator it;

		// trim any links to parents that are redundant with
		// a GRANDPARENT link
		// if any trimmed link is a GIVEN link, it is redundant

		boolean oldMethod = true;

		Collection<Link> parents = linkDatabase.getParents(link.getChild());
		it = parents.iterator();
		// for each parent link
		while (it.hasNext()) {
			Link parentLink = (Link) it.next();

			if (parentLink.equals(link)) {
				continue;
			}

			if (!(parentLink.getType().equals(link.getType()) || parentLink
					.getType().equals(OBOProperty.IS_A)))
				continue;
			boolean sawType = parentLink.getType().equals(link.getType());

			Iterator it2 = linkDatabase.getParents(parentLink.getParent())
					.iterator();

			// for each grandparent link accessible via the current
			// parent link...
			while (it2.hasNext()) {
				Link gpLink = (Link) it2.next();
				if (TermUtil.isIntersection(gpLink))
					continue;
				// see if the grandparent link has the same type
				// and parent as the current link. if it does,
				// the current link is redundant with the grandparent
				// link and should be removed

				if (link.getParent().equals(gpLink.getParent())
						&& (((!sawType || link.getType().isTransitive()) && link
								.getType().equals(gpLink.getType())) || (sawType && gpLink
								.getType().equals(OBOProperty.IS_A)))) {
					return true;
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

	public static boolean isRedundant(ReasonedLinkDatabase reasoner, Link link) {
		if (TermUtil.isIntersection(link))
			return false;
		Collection<Explanation> exps = reasoner.getExplanations(link);
		if (exps.size() == 1)
			return false;
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
	
	public static Collection<OBOClass> getDifferentiaByType(OBOClass oboClass, OBOProperty prop) {
		Collection<OBOClass> out = new LinkedList<OBOClass>();
		for (Link parentLink : oboClass.getParents()) {
			if (parentLink.getType().equals(prop)
					&& TermUtil.isIntersection(parentLink))
				out.add(TermUtil.castToClass(parentLink.getParent()));
		}
		return out;

	}

	public static Collection<OBOClass> getDifferentiaByType(OBOClass oboClass, String propName) {
		Collection<OBOClass> out = new LinkedList<OBOClass>();
		for (Link parentLink : oboClass.getParents()) {
			if (parentLink.getType().getName().equals(propName)
					&& TermUtil.isIntersection(parentLink))
				out.add(TermUtil.castToClass(parentLink.getParent()));
		}
		return out;

	}


}
