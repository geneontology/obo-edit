package org.obo.reasoner.impl;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.util.FastSuperset;
import org.bbop.util.ProgressValued;
import org.bbop.util.Superset;
import org.bbop.util.TinySet;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.AbstractLinkDatabase;
import org.obo.datamodel.impl.DefaultMutableLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerListener;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

import java.awt.event.*;

import java.util.*;

public class ForwardChainingReasoner extends AbstractLinkDatabase implements
		ReasonedLinkDatabase, ProgressValued {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5046369728943561325L;

	protected LinkDatabase linkDatabase;

	protected MutableLinkDatabase impliedLinkDatabase;

	protected Map<LinkedObject, Collection<Link>> intersectionMap = new LinkedHashMap<LinkedObject, Collection<Link>>();

	protected Map<Link, Collection<Explanation>> explanationMap = new HashMap<Link, Collection<Explanation>>();

	protected Map<Link, Collection<Explanation>> explanationDeps = new HashMap<Link, Collection<Explanation>>();

	protected Number progressVal;
	protected String progressString;

	protected boolean useTinySet = false;
	protected boolean evidenceLinkedListMode = true;
	protected boolean neverFindMode = true;
	protected boolean cancelled = false;
	protected boolean isRunning = false;

	public ForwardChainingReasoner(MutableLinkDatabase impliedLinkDatabase) {
		setImpliedLinkDatabase(impliedLinkDatabase);
	}

	protected void setImpliedLinkDatabase(
			MutableLinkDatabase impliedLinkDatabase) {
		this.impliedLinkDatabase = impliedLinkDatabase;
	}
	
	public Map<Link, Collection<Explanation>>  getExplanationMap() {
		return explanationMap;
	}
	
	public Map<Link, Collection<Explanation>>  getExplanationDepsMap() {
		return explanationDeps;
	}

	public ForwardChainingReasoner() {
		this(createImpliedLinkDatabase(null));
	}

	protected Map<LinkedObject, Collection<Link>> getIntersectionMap() {
		return intersectionMap;
	}

	protected void clearIntersectionMap() {
		intersectionMap.clear();
	}

	protected static MutableLinkDatabase createImpliedLinkDatabase(
			LinkDatabase linkDatabase) {
		/*
		 * return new FixedCacheMutableLinkDatabase( new
		 * DefaultMutableLinkDatabase(true, false), false);
		 */
		return new DefaultMutableLinkDatabase(true);
	}

	public void addActionListener(ActionListener listener) {
	}

	public void removeActionListener(ActionListener listener) {
	}

	protected Link findLink(LinkedObject child, OBOProperty type,
			LinkedObject parent) {
		return findLink(child, type, parent, false);
	}

	/**
	 * Does a hashed search of all the links to find a link with a particular
	 * child, type, and parent. Note that this uses a scratch link to do the
	 * lookup, but will return the real link we're looking for, evidence and
	 * all.
	 */
	protected OBORestriction lookupLink = new OBORestrictionImpl();

	protected Link findLink(LinkedObject child, OBOProperty type,
			LinkedObject parent, boolean completes) {
		findAttempts++;
		long time = System.currentTimeMillis();
		for (Link link : getParents(child)) {
			// TODO: Fix this problem
			if (isSubPropertyOf(link.getType(), type)
					&& link.getParent().equals(parent)
					&& TermUtil.isIntersection(link) == completes) {
				findLinkTime += System.currentTimeMillis() - time;
				findHits++;
				return link;
			}
		}
		findLinkTime += System.currentTimeMillis() - time;
		return null;
	}

	int findAttempts = 0;
	int findHits = 0;

	protected boolean linkExists(Link link) {
		long time = System.currentTimeMillis();
		Collection<Link> parents = getParents(link.getChild());
		/*
		 * boolean contains = parents.contains(link); boolean myContains =
		 * false; for(Link p: parents) { if
		 * (p.getChild().getID().equals(link.getChild().getID()) &&
		 * p.getParent().getID().equals(link.getParent().getID()) &&
		 * p.getType().getID().equals(link.getType().getID())) { myContains =
		 * true; } } if (contains != myContains) { System.err.println("!!!
		 * contains() method is broken on "+link+", parents = "+parents); }
		 */
		existsTime += System.currentTimeMillis() - time;
		return parents.contains(link);
	}

	protected Link findOrCreateLink(LinkedObject child, OBOProperty type,
			LinkedObject parent) {
		if (neverFindMode) {
			return new OBORestrictionImpl(child, type, parent, true);
		} else {
			Link out = findLink(child, type, parent);
			if (out == null) {
				out = new OBORestrictionImpl(child, type, parent, true);
			}
			return out;
		}
		//
	}

	boolean debugMode = false;

	protected void internalAddLink(Link link) {
		if (link.getParent().equals(link.getChild()))
			System.err.println("Created new cycle!");
		long time = System.currentTimeMillis();
		newLinks++;
		if (TermUtil.isIntersection(link)) {
			Collection<Link> completeParents = intersectionMap.get(link
					.getChild());
			if (completeParents == null) {
				completeParents = new LinkedHashSet<Link>();
				intersectionMap.put(link.getChild(), completeParents);
			}
			completeParents.add(link);
			doGenusDifferentiaImplications(link);
		} else {
			impliedLinkDatabase.addParent(link);
		}
		addLinkTime += System.currentTimeMillis() - time;
	}

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
		setImpliedLinkDatabase(createImpliedLinkDatabase(linkDatabase));
		// recache();
	}

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	public Collection<IdentifiedObject> getObjects() {
		return linkDatabase.getObjects();
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		/*
		 * Superset<Link> s = new FastSuperset<Link>();
		 * s.addSubset(linkDatabase.getChildren(lo));
		 * s.addSubset(impliedLinkDatabase.getChildren(lo)); return s;
		 */
		Collection<Link> given = linkDatabase.getChildren(lo);
		Collection<Link> impliedChildren = impliedLinkDatabase.getChildren(lo);
		Collection<Link> out = new LinkedHashSet<Link>(given.size()
				+ impliedChildren.size());
		out.addAll(given);
		out.addAll(impliedChildren);
		return out;
	}

	public Collection<Link> getParents(LinkedObject lo) {
		/*
		 * Superset<Link> s = new FastSuperset<Link>(); Collection<Link>
		 * given = linkDatabase.getParents(lo); Collection<Link> implied =
		 * impliedLinkDatabase.getParents(lo); s.addSubset(given);
		 * s.addSubset(implied); return s;
		 */
		Collection<Link> given = linkDatabase.getParents(lo);
		Collection<Link> impliedParents = impliedLinkDatabase.getParents(lo);
		Collection<Link> out = new LinkedHashSet<Link>(given.size()
				+ impliedParents.size());
		out.addAll(given);
		out.addAll(impliedParents);
		return out;
	}

	/* Make this more general */
	public boolean isSubclass(LinkedObject a, LinkedObject b) {
		if (a.equals(b))
			return true;
		for (Link link : getParents(a)) {
			if (isSubPropertyOf(link.getType(), OBOProperty.IS_A)
					&& link.getParent().equals(b))
				return true;
		}
		return false;
	}

	public Link hasRelationship(LinkedObject a, OBOProperty b, LinkedObject c) {
		Iterator<Link> it = getParents(a).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isSubPropertyOf(link.getType(), b)
					&& link.getParent().equals(c)) {
				return link;
			}
		}
		return null;
	}

	public Set<LinkedObject> getParentsOfType(LinkedObject a, OBOProperty b) {
		Set<LinkedObject> out = createSet();
		Iterator<Link> it = getParents(a).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isSubPropertyOf(link.getType(), b)) {
				out.add(link.getParent());
			}
		}
		return out;
	}

	public boolean isInstanceOf(Instance a, OBOClass b) {
		return false;
	}

	public boolean isSubPropertyOf(OBOProperty a, OBOProperty b) {
		return isSubclass(a, b);
	}

	public boolean isSubclassOf(OBOClass a, OBOClass b) {
		return isSubclass((LinkedObject) a, (LinkedObject) b);
	}

	public boolean isRedundant(Link link) {
		return ReasonerUtil.isRedundant(this, link);
	}

	public void addLink(Link link) {
		// fireStart();
		if (link instanceof OBORestriction
				&& ((OBORestriction) link).completes()) {
			// adding a new complete link can cause some implied
			// relationships to get removed, because the complete
			// definition is now narrower

			// so if you add a new complete link, delete all relationships
			// that rely on a complete definition from link.getChild()

			Collection<CompletenessExplanation> completeExplanations = new LinkedList<CompletenessExplanation>();
			for (Link childLink : getChildren(link.getChild())) {
				// Iterator it = getChildren(link.getChild()).iterator();
				// while (it.hasNext()) {
				// Link childLink = (Link) it.next();
				for (Explanation exp : getExplanations(childLink)) {
					if (exp.getExplanationType().equals(
							ExplanationType.INTERSECTION)) {
						completeExplanations.add((CompletenessExplanation) exp);
					}
				}
			}
			for (AbstractExplanation exp : completeExplanations) {
				// it = completeExplanations.iterator();
				// while (it.hasNext()) {
				// AbstractExplanation exp = (AbstractExplanation) it.next();
				reasonRemoval(exp, null);
			}
			internalAddLink(link);
		} else {
			/*
			 * internalAddLink(link); explain(link,
			 * Explanation.GIVEN_EXPLANATION);
			 */
			pushNewLink(link);
		}
		doCompleteChecks();
		// fireDone();
		debugMode = false;
	}

	public void removeLink(Link link) {
		// fireStart();
		reasonRemoval(link);
		doCompleteChecks();
		// fireDone();
	}

	protected Collection<Explanation> getDependentExplanations(Link link) {
		/*
		 * Collection<Explanation> out = new HashSet<Explanation>(); for (Link
		 * explainMe : explanationMap.keySet()) { for (Explanation exp :
		 * explanationMap.get(explainMe)) { if
		 * (exp.getEvidence().contains(link)) { out.add(exp); } } } return out;
		 */
		Collection<Explanation> out = explanationDeps.get(link);
		if (out == null)
			return Collections.emptySet();
		else
			return out;
	}

	protected void reasonRemoval(Link link) {
		// actually remove the dead link from the various caches
		impliedLinkDatabase.removeParent(link);
		explanationMap.remove(link);
		if (link instanceof OBORestriction
				&& ((OBORestriction) link).completes()) {
			Collection<Link> completeParents = intersectionMap.get(link
					.getChild());
			if (completeParents != null) {
				completeParents.remove(link);
				if (completeParents.size() == 0) {
					intersectionMap.remove(link.getChild());
				}
			}
		}

		// find all the explanations that depend on the dead link
		Collection<Explanation> deps = getDependentExplanations(link);
		
		for (Explanation exp : new ArrayList<Explanation>(deps)) {
			// remove the now-defunct link as supporting evidence for the
			// dependent
			// explanation
			boolean dead = exp.removeEvidence(link);
			// if dead == true, it means that removing the defunct link
			// invalidated
			// the explanation, so that explanation needs to be removed
			if (dead) {
				// ditch the explanation
				reasonRemoval(exp, link);
			}
		}
	}

	protected void reasonRemoval(Explanation exp, Link justRemoved) {
		Collection<Link> evidenceLinks = new ArrayList(exp.getEvidence());
		if (justRemoved != null)
			evidenceLinks.add(justRemoved);
		// Since we're removing this explanation, this explanation no longer
		// relies
		// on any supporting links, so it needs to be removed from
		// explanationDeps.
		// Therefore, iterate through all the other links that support this
		// evidence
		// For each of those links, remove the now-defunct explanation from the
		// list
		// of explanations that relies on that link
		for (Link link : evidenceLinks) {
			Collection<Explanation> exps = explanationDeps.get(link);
			if (exps != null) {
				exps.remove(exp);
				if (exps.size() == 0)
					explanationDeps.remove(link);
			}
		}

		// get the object that was explained by this explanation
		PathCapable explainedObject = exp.getExplainedObject();

		// Fetch all the explanations (including the one we're about to remove)
		// for explainedObject
		Collection<Explanation> exps = getExplanations(explainedObject);
		// remove the now-defunct explanation from the set of explanations
		exps.remove(exp);

		// if there are no explanations left for explainedObject, it needs
		// to be removed too
		if (exps.size() == 0)
			reasonRemoval(exp.getExplainedObject());
	}

	protected void reasonRemoval(PathCapable pc) {
		if (pc instanceof Link)
			reasonRemoval((Link) pc);
	}

	protected int newLinks = 0;

	protected boolean recaching = false;

	public long recache() {
		isRunning = true;
		cancelled = false;
		addLinkTime = 0;
		existsTime = 0;
		explainTime = 0;
		reasonLinkTime = 0;
		findLinkTime = 0;
		expMapSetupTime = 0;
		depExpMapSetupTime = 0;
		findAttempts = 0;
		findHits = 0;
		fireStart();
		recaching = true;
		long time = System.currentTimeMillis();

		impliedLinkDatabase.clear();
		clearIntersectionMap();
		explanationMap.clear();
		explanationDeps.clear();
		newLinks = 0;
		Collection<LinkedObject> classes = new LinkedList<LinkedObject>();
		Collection<LinkedObject> properties = new LinkedList<LinkedObject>();

		int objCount = TermUtil.getObjectCount(linkDatabase);
		Iterator<IdentifiedObject> it = linkDatabase.getObjects().iterator();
		for (int i = 0; it.hasNext(); i++) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (cancelled)
				return System.currentTimeMillis() - time;
			if (io == null)
				System.err.println("NULL OBJECT IN DEFAULT OBJECT DATABASE!!!");
			if (io instanceof OBOProperty)
				properties.add((LinkedObject) io);
			else if (io instanceof OBOClass)
				classes.add((LinkedObject) io);
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;

				Iterator<? extends Relationship> it2 = linkDatabase.getParents(
						lo).iterator();
				while (it2.hasNext()) {
					Relationship rel = it2.next();
					if (!(rel instanceof Link))
						continue;
					Link link = (Link) rel;
					if (cancelled)
						return System.currentTimeMillis() - time;

					if (TermUtil.isIntersection(link)) {
						Collection<Link> completeParents = intersectionMap
								.get(link.getChild());
						if (completeParents == null) {
							completeParents = createSet();
							intersectionMap.put(link.getChild(),
									completeParents);
						}
						completeParents.add(link);
					}
				}
			}
			showProgress(100 * i / objCount, "Initializing database...");
		}

		// disable any complete def that contains dangling parents
		Iterator<LinkedObject> it2 = intersectionMap.keySet().iterator();
		while (it2.hasNext()) {
			LinkedObject lo = it2.next();
			if (cancelled)
				return System.currentTimeMillis() - time;
			Collection<Link> parents = intersectionMap.get(lo);
			for (Link link : parents) {
				if (cancelled)
					return System.currentTimeMillis() - time;
				if (TermUtil.isDangling(link)) {
					it2.remove();
					break;
				}
			}
		}

		showProgress(0, "Discovering genus/diff implications...");
		doGenusDifferentiaImplications();

		showProgress(0, "Calculating basic symmetry implications...");
		doSymmetryImplications();

		showProgress(0, "Calculating inversion implications...");
		doInversionImplications();

		if (cancelled)
			return System.currentTimeMillis() - time;

		showProgress(0, "Calculating transitive closure: properties...");
		doTransitiveClosure(properties);

		if (cancelled)
			return System.currentTimeMillis() - time;

		showProgress(0, "Calculating transitive closure: classes...");
		doTransitiveClosure(classes);

		if (cancelled)
			return System.currentTimeMillis() - time;

		properties = null;
		classes = null;

		doCompleteChecks();

		if (cancelled)
			return System.currentTimeMillis() - time;

		time = System.currentTimeMillis() - time;
		recaching = false;
		fireDone();
		isRunning = false;
		return time;
	}

	public void cancel() {
		cancelled = true;
		isRunning = false;
	}

	public boolean isRunning() {
		return isRunning;
	}

	public boolean isCancelled() {
		return cancelled;
	}

	protected Set<String> trash;

	protected void doGenusDifferentiaImplications() {

		for (LinkedObject lo : getIntersectionMap().keySet()) {
			Collection<Link> c = getIntersectionMap().get(lo);

			for (Link or : c) {
				if (cancelled)
					return;
				doGenusDifferentiaImplications(or);
			}
		}
	}

	protected void doGenusDifferentiaImplications(Link or) {
		// System.err.println("reasoning genus/diff implications of "+or);
		if (isSubPropertyOf(or.getType(), OBOProperty.IS_A)) {
			Link link = findOrCreateLink(or.getChild(), OBOProperty.IS_A, or
					.getParent());
			internalAddLink(link);

			AbstractExplanation explanation = new GenusExplanation(or);
			explain(link, explanation);
		} else {
			Link link = findOrCreateLink(or.getChild(), or.getType(), or
					.getParent());
			internalAddLink(link);
			AbstractExplanation explanation = new DifferentiaExplanation(or);
			explain(link, explanation);
		}
	}

	protected void doSymmetryImplications() {
		Iterator<Link> it = TermUtil.getAllLinks(linkDatabase);
		while (it.hasNext()) {
			Link l = it.next();
			if (!TermUtil.isIntersection(l) && l.getType().isSymmetric()) {
				Link link = findOrCreateLink(l.getParent(), l.getType(), l
						.getChild());
				internalAddLink(link);
				AbstractExplanation explanation = new SymmetryExplanation(l,
						link);
				explain(link, explanation);
			}
		}
	}

	protected void doInversionImplications() {
		Iterator<Link> it = TermUtil.getAllLinks(linkDatabase);
		while (it.hasNext()) {
			Link l = it.next();
			if (!TermUtil.isIntersection(l)
					&& l.getType().isAlwaysImpliesInverse()) {
				Collection<LinkedObject> inverseParents = getParentsOfType(l
						.getType(), OBOProperty.INVERSE_OF);
				for (LinkedObject lo : inverseParents) {
					if (lo instanceof OBOProperty) {
						OBOProperty type = (OBOProperty) lo;

						Link link = findOrCreateLink(l.getParent(), type, l
								.getChild());
						internalAddLink(link);
						AbstractExplanation explanation = new AlwaysImpliesInverseExplanation(
								link, l);
						explain(link, explanation);
					}
				}
			}
		}
	}

	protected void doCompleteChecks() {
		int index = 1;
		do {
			newLinks = 0;
			showProgress(0, "Calculating completeness: pass " + index + "...");

			checkCompleteDefs();
			index++;
		} while (newLinks > 0);
	}

	protected void checkCompleteDefs() {
		int i = 0;

		Collection<LinkedObject> checkedThese = new HashSet<LinkedObject>();
		int mapSize = getIntersectionMap().size();
		for (LinkedObject lo : getIntersectionMap().keySet()) {
			if (cancelled)
				return;

			Map<LinkedObject, Collection<CompletenessMatch>> candidates = new LinkedHashMap<LinkedObject, Collection<CompletenessMatch>>();

			showProgress(i++ * 100 / mapSize,
					"Checking intersections for object " + i + " of " + mapSize);
			Collection<Link> completeLinks = new LinkedList<Link>();
			completeLinks.addAll(getIntersectionMap().get(lo));

			// find the link that has the smallest number of children
			// attached to the parent; this will give us the smallest
			// set of candidate terms
			Link starterLink = null;
			int minSetSize = Integer.MAX_VALUE;
			for (Link completeLink : completeLinks) {
				int childCount = TermUtil.getChildCount(this, completeLink
						.getParent());
				if (childCount < minSetSize) {
					starterLink = completeLink;
					minSetSize = childCount;
				}
			}
			completeLinks.remove(starterLink);

			// System.err.println("checking complete definition for
			// "+starterLink.getChild()+", lo = "+lo);
			checkedThese.add(starterLink.getChild());

			if (starterLink == null) {
				System.err.println("UNEXPECTED CONDITION: "
						+ "EMPTY COMPLETE DEF!");
			} else {

				Link completeLink = starterLink;
				Collection<Link> children = getChildren(completeLink
						.getParent());
				for (Link candidateLink : children) {
					if (cancelled)
						return;
					// all complete defs match themselves
					// there's no need to create an explicit link for that,
					// so we don't put them into the initial collection
					if (candidateLink.getChild().equals(lo))
						continue;
					// ignore intersection links
					if (TermUtil.isIntersection(candidateLink))
						continue;
					if (findLink(candidateLink.getChild(), completeLink
							.getType(), completeLink.getParent()) != null) {
						Collection<CompletenessMatch> evidence = candidates
								.get(candidateLink.getChild());
						if (evidence == null) {
							evidence = createSet();
							candidates.put(candidateLink.getChild(), evidence);
						}
						evidence.add(new CompletenessMatch(candidateLink,
								completeLink));

					}
				}
			}

			Iterator<LinkedObject> it2 = candidates.keySet().iterator();
			while (it2.hasNext()) {
				if (cancelled)
					return;
				LinkedObject candidate = it2.next();
				for (Link completeLink : completeLinks) {
					Link candidateLink = findLink(candidate, completeLink
							.getType(), completeLink.getParent());

					if (candidateLink != null) {
						Collection<CompletenessMatch> evidence = candidates
								.get(candidate);
						if (evidence == null) {
							evidence = createSet();
							candidates.put(candidate, evidence);
						}
						evidence.add(new CompletenessMatch(candidateLink,
								completeLink));
					} else {
						it2.remove();
						break;
					}
				}
			}

			it2 = candidates.keySet().iterator();
			while (it2.hasNext()) {
				if (cancelled)
					return;
				LinkedObject candidate = (LinkedObject) it2.next();

				Collection<CompletenessMatch> matches = candidates
						.get(candidate);

				LinkedObject matchParent = null;

				CompletenessExplanation exp = new CompletenessExplanation();

				for (CompletenessMatch cm : matches) {
					if (cancelled)
						return;
					matchParent = cm.getCompletenessLink().getChild();
					exp.addMatch(cm);
				}

				// all complete defs match their genus (by the GENUS rule)
				// but sometimes they also match their genus via the
				// completeness definition. That can lead to a proliferation
				// of confusing explanations, so we just back out immediately
				// if the candidate is the genus term.
				LinkedObject candidateGenus = getGenus(candidate);
				if (candidateGenus != null
						&& matchParent.equals(candidateGenus)) {
					continue;
				}

				Link newRel = findOrCreateLink(candidate, OBOProperty.IS_A,
						matchParent);

				if (!linkExists(newRel)) {
					internalAddLink(newRel);
					pushNewLink(newRel);
				}

				explain(newRel, exp);
			}
		}
	}

	public LinkedObject getGenus(LinkedObject candidate) {
		for (Link link : linkDatabase.getParents(candidate)) {
			if (TermUtil.isIntersection(link)
					&& isSubPropertyOf(link.getType(), OBOProperty.IS_A))
				return link.getParent();
		}
		return null;
	}

	protected void cacheAllParents(LinkedObject lo,
			Collection<LinkedObject> cached) {
		if (cached.contains(lo))
			return;
		cached.add(lo);

		Collection<Link> parents = new LinkedList<Link>(getParents(lo));
		for (Link link : parents) {
			if (cancelled)
				return;

			cacheAllParents(link.getParent(), cached);
		}

		for (Link link : parents) {
			if (cancelled)
				return;
			if (TermUtil.isIntersection(link))
				continue;

			Collection<Link> grandParents = new LinkedList<Link>(
					getParents(link.getParent()));
			for (Link gpLink : grandParents) {
				if (TermUtil.isIntersection(gpLink))
					continue;
				Link newRel = reasonLink(link, gpLink);

				if (newRel != null) {
					if (newRel.getParent().equals(newRel.getChild())
							&& isSubPropertyOf(newRel.getType(),
									OBOProperty.IS_A))
						continue;

					if (!linkExists(newRel)) {
						internalAddLink(newRel);
					}

					AbstractExplanation explanation = new TransitivityExplanation(
							link, gpLink);
					explain(newRel, explanation);
				}
			}
		}
	}

	protected OBORestriction tempLink = new OBORestrictionImpl(
			(LinkedObject) null, (OBOProperty) null, (LinkedObject) null);

	// TODO add this info
	long reasonLinkTime = 0;
	public long explainTime = 0;
	long addLinkTime = 0;
	long existsTime = 0;
	long findLinkTime = 0;
	long expMapSetupTime = 0;
	long depExpMapSetupTime = 0;

	protected Link reasonLink(Link link, Link gpLink) {
		long time = System.currentTimeMillis();
		Link newRel = null;
		if (ReasonerUtil.generateTransitiveImplication(this, tempLink, link,
				gpLink)) {
			newRel = findOrCreateLink(tempLink.getChild(), tempLink.getType(),
					tempLink.getParent());
		}
		reasonLinkTime += System.currentTimeMillis() - time;
		return newRel;
	}

	protected void doTransitiveClosure(Collection<LinkedObject> objects) {
		Collection<LinkedObject> seenem = new HashSet<LinkedObject>();
		Iterator<LinkedObject> it = objects.iterator();
		for (int i = 0; it.hasNext(); i++) {
			if (cancelled)
				return;
			LinkedObject lo = it.next();
			cacheAllParents(lo, seenem);
			showProgress(100 * i / objects.size());
		}
	}

	public void pushLinksDown(Collection<Link> links, Link throughLink) {
		for (Link link : links) {
			if (link.equals(throughLink) || TermUtil.isIntersection(link))
				continue;

			OBORestriction newRel = (OBORestriction) reasonLink(throughLink,
					link);

			if (newRel != null) {
				if (!linkExists(newRel)) {
					internalAddLink(newRel);
				}

				AbstractExplanation explanation = new TransitivityExplanation(
						throughLink, link);
				explain(newRel, explanation);

				pushLink(newRel, new HashSet<Link>());
			}
		}
	}

	protected void pushNewLink(Link link) {
		pushLink(link, new HashSet<Link>());
		Collection<Link> parents = new LinkedList<Link>();
		parents.addAll(getParents(link.getParent()));
		pushLinksDown(parents, link);
	}

	public void pushLink(Link link, Collection<Link> seenem) {
		// System.err.println("pushing link : "+link);
		if (TermUtil.isIntersection(link))
			System.err.println("shouldn't push an intersection");

		if (seenem.contains(link)) {
			return;
		} else
			seenem.add(link);

		LinkedObject target = link.getChild();

		Collection<Link> children = new LinkedList<Link>();
		children.addAll(getChildren(target));

		Iterator<Link> it = children.iterator();
		while (it.hasNext()) {
			Link childLink = (Link) it.next();
			if (TermUtil.isIntersection(childLink))
				continue;
			OBORestriction newRel = (OBORestriction) reasonLink(childLink, link);

			if (newRel != null) {
				boolean foundLink = linkExists(newRel);

				if (!foundLink) {
					internalAddLink(newRel);
				}

				AbstractExplanation explanation = new TransitivityExplanation(
						childLink, link);
				explain(newRel, explanation);

				if (!foundLink)
					pushLink(newRel, seenem);
			}
		}
	}

	public Collection<Explanation> getExplanations(PathCapable pc) {
		if (!(pc instanceof Link))
			return Collections.emptySet();
		Link link = (Link) pc;
		Collection<Explanation> explanations = null;
		explanations = explanationMap.get(link);

		if (explanations == null) {
			if (TermUtil.isImplied(link)) {
				return Collections
						.singleton((Explanation) new ExternallyImpliedExplanation(
								link));
			} else
				return Collections
						.singleton((Explanation) new GivenExplanation(link));
		} else {
			// Anything that is in the original database needs a "given"
			// explanation
			// added to it
			if (TermUtil.containsLink(linkDatabase, link)) {
				Collection<Explanation> out = new LinkedList<Explanation>(
						explanations);
				out.add(new GivenExplanation(link));
				return out;
			} else
				return explanations;
		}
	}

	protected <T> Set<T> createSet() {
		if (useTinySet)
			return new TinySet<T>();
		else
			return new HashSet<T>();
	}

	protected Collection<Explanation> getDependentExplanations(PathCapable pc) {
		Collection<Explanation> depEx = explanationDeps.get(pc);
		if (depEx == null)
			return Collections.emptySet();
		else
			return depEx;
	}

	protected void explain(Link link, Explanation explanation) {
		if (explanation.getExplanationType().equals(ExplanationType.GIVEN))
			throw new IllegalArgumentException();
		long time = System.currentTimeMillis();
		if (explanation instanceof AbstractExplanation) {
			((AbstractExplanation) explanation).setExplainedLink(link);
		}
		long time2 = System.currentTimeMillis();
		Collection<Explanation> explanations = explanationMap.get(link);
		if (explanations == null) {
			if (evidenceLinkedListMode)
				explanations = new LinkedList<Explanation>();
			else
				explanations = createSet();
			explanationMap.put(link, explanations);
		}
		expMapSetupTime += System.currentTimeMillis() - time2;
		time2 = System.currentTimeMillis();
		for (Link evidence : explanation.getEvidence()) {
			Collection<Explanation> depExp = explanationDeps.get(evidence);
			if (depExp == null) {
				if (evidenceLinkedListMode)
					depExp = new LinkedList<Explanation>();
				else
					depExp = createSet();
				explanationDeps.put(evidence, depExp);

			}
			depExp.add(explanation);
		}
		depExpMapSetupTime += System.currentTimeMillis() - time2;
		explanations.add(explanation);
		explainTime += System.currentTimeMillis() - time;
	}

	protected Collection<ReasonerListener> reasonerListeners = new LinkedList<ReasonerListener>();

	public static boolean checkRecache = false;

	public static Link weirdLink;

	public static void main(String[] args) throws DataAdapterException {
		OBOSession session = TermUtil
				.getSession("/Users/jrichter/ontology/gene_ontology_edit.obo");
		ForwardChainingReasoner reasoner = new ForwardChainingReasoner();
		reasoner.setLinkDatabase(session.getLinkDatabase());
		long time;

		for (int i = 0; i < 10; i++) {
			/*
			 * time = System.currentTimeMillis(); reasoner.useTinySet = true;
			 * reasoner.neverFindMode = false; reasoner.useLinkLookupTable =
			 * false; reasoner.recache(); System.err.println("reasoned in " +
			 * (System.currentTimeMillis() - time) + " using TinySet");
			 * System.err.println(" addlinktime = " + reasoner.addLinkTime);
			 * System.err.println(" existsTime = " + reasoner.existsTime);
			 * System.err.println(" explainTime = " + reasoner.explainTime);
			 * System.err.println(" reasonLinkTime = " +
			 * reasoner.reasonLinkTime); System.err.println(" findLinkTime = " +
			 * reasoner.findLinkTime); System.err.println(" findAttempts = " +
			 * reasoner.findAttempts); System.err.println(" findHits = " +
			 * reasoner.findHits);
			 */

			reasoner.useTinySet = true;
			reasoner.neverFindMode = false;
			reasoner.evidenceLinkedListMode = false;
			time = System.currentTimeMillis();
			reasoner.recache();
			System.err.println("reasoned in "
					+ (System.currentTimeMillis() - time) + " using defaults");
			System.err.println("  addlinktime = " + reasoner.addLinkTime);
			System.err.println("  existsTime = " + reasoner.existsTime);
			System.err.println("  explainTime = " + reasoner.explainTime);
			System.err.println("     depExpMapSetupTime = "
					+ reasoner.depExpMapSetupTime);
			System.err.println("     expMapSetupTime = "
					+ reasoner.expMapSetupTime);
			/*
			 * System.err.println(" reasonLinkTime = " +
			 * reasoner.reasonLinkTime); System.err.println(" findLinkTime = " +
			 * reasoner.findLinkTime); System.err.println(" findAttempts = " +
			 * reasoner.findAttempts); System.err.println(" findHits = " +
			 * reasoner.findHits);
			 * 
			 * reasoner.useTinySet = false; reasoner.neverFindMode = false;
			 * reasoner.evidenceLinkedListMode = false; time =
			 * System.currentTimeMillis(); reasoner.recache();
			 * System.err.println("reasoned in " + (System.currentTimeMillis() -
			 * time) + " using HashSet"); System.err.println(" addlinktime = " +
			 * reasoner.addLinkTime); System.err.println(" existsTime = " +
			 * reasoner.existsTime); System.err.println(" explainTime = " +
			 * reasoner.explainTime); System.err.println(" depExpMapSetupTime = " +
			 * reasoner.depExpMapSetupTime); System.err.println("
			 * expMapSetupTime = " + reasoner.expMapSetupTime);
			 * 
			 * System.err.println(" reasonLinkTime = " +
			 * reasoner.reasonLinkTime); System.err.println(" findLinkTime = " +
			 * reasoner.findLinkTime); System.err.println(" findAttempts = " +
			 * reasoner.findAttempts); System.err.println(" findHits = " +
			 * reasoner.findHits);
			 * 
			 * reasoner.useTinySet = false; reasoner.neverFindMode = false;
			 * reasoner.evidenceLinkedListMode = true; time =
			 * System.currentTimeMillis(); reasoner.recache();
			 * System.err.println("reasoned in " + (System.currentTimeMillis() -
			 * time) + " using linked list mode"); System.err.println("
			 * addlinktime = " + reasoner.addLinkTime); System.err.println("
			 * existsTime = " + reasoner.existsTime); System.err.println("
			 * explainTime = " + reasoner.explainTime); System.err.println("
			 * depExpMapSetupTime = " + reasoner.depExpMapSetupTime);
			 * System.err.println(" expMapSetupTime = " +
			 * reasoner.expMapSetupTime);
			 * 
			 * System.err.println(" reasonLinkTime = " +
			 * reasoner.reasonLinkTime); System.err.println(" findLinkTime = " +
			 * reasoner.findLinkTime); System.err.println(" findAttempts = " +
			 * reasoner.findAttempts); System.err.println(" findHits = " +
			 * reasoner.findHits);
			 * 
			 * reasoner.useTinySet = false; reasoner.neverFindMode = true;
			 * reasoner.evidenceLinkedListMode = false; time =
			 * System.currentTimeMillis(); reasoner.recache();
			 * System.err.println("reasoned in " + (System.currentTimeMillis() -
			 * time) + " using never find mode"); System.err.println("
			 * addlinktime = " + reasoner.addLinkTime); System.err.println("
			 * existsTime = " + reasoner.existsTime); System.err.println("
			 * explainTime = " + reasoner.explainTime); System.err.println("
			 * reasonLinkTime = " + reasoner.reasonLinkTime);
			 * System.err.println(" findLinkTime = " + reasoner.findLinkTime);
			 */
			reasoner.useTinySet = false;
			reasoner.neverFindMode = true;
			reasoner.evidenceLinkedListMode = true;
			time = System.currentTimeMillis();
			reasoner.recache();
			System.err.println("reasoned in "
					+ (System.currentTimeMillis() - time)
					+ " using never find and linked list mode");
			System.err.println("  addlinktime = " + reasoner.addLinkTime);
			System.err.println("  existsTime = " + reasoner.existsTime);
			System.err.println("  explainTime = " + reasoner.explainTime);
			System.err.println("     depExpMapSetupTime = "
					+ reasoner.depExpMapSetupTime);
			System.err.println("     expMapSetupTime = "
					+ reasoner.expMapSetupTime);

			System.err.println("  reasonLinkTime = " + reasoner.reasonLinkTime);
			System.err.println("  findLinkTime = " + reasoner.findLinkTime);
			System.err.println("  findAttempts = " + reasoner.findAttempts);
			System.err.println("  findHits = " + reasoner.findHits);
		}

		double objCount = session.getObjects().size();
		double linkCount = 0;
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof LinkedObject) {
				linkCount += ((LinkedObject) io).getParents().size();
			}
		}
		System.err.println("original avg. link count " + linkCount / objCount);
		linkCount = 0;
		for (IdentifiedObject io : reasoner.getObjects()) {
			if (io instanceof LinkedObject) {
				linkCount += reasoner.getParents((LinkedObject) io).size();
			}
		}

		System.err.println("reasoned avg. link count " + linkCount / objCount);

	}

	protected void fireDone() {
		for (ReasonerListener reasonerListener : reasonerListeners) {
			reasonerListener.reasoningFinished();
		}
	}

	protected void fireStart() {
		for (ReasonerListener reasonerListener : reasonerListeners) {
			reasonerListener.reasoningStarted();
		}
	}

	public void addReasonerListener(ReasonerListener listener) {
		reasonerListeners.add(listener);
	}

	public void removeReasonerListener(ReasonerListener listener) {
		reasonerListeners.remove(listener);
	}

	protected void showProgress(int percent, String message) {
		progressVal = percent;
		progressString = message;
	}

	protected void showProgress(int percent) {
		progressVal = percent;
	}

	public IdentifiedObject getObject(String id) {
		return linkDatabase.getObject(id);
	}

	public String getProgressString() {
		return progressString;
	}

	public Number getProgressValue() {
		return progressVal;
	}
}
