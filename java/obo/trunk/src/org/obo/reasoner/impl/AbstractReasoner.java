package org.obo.reasoner.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Set;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiHashSetMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MutableLinkDatabase;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.impl.AbstractLinkDatabase;
import org.obo.datamodel.impl.DefaultMutableLinkDatabase;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerListener;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;

public abstract class AbstractReasoner extends AbstractLinkDatabase implements
		ReasonedLinkDatabase {

	protected MultiMap<Link, Explanation> explanationMap;

	protected MultiMap<Link, Explanation> explanationDeps;

	protected LinkDatabase linkDatabase;

	protected MutableLinkDatabase impliedLinkDatabase;

	protected boolean cancelled = false;

	protected boolean running = false;

	protected boolean storeGivenLinks = false;

	protected String progressString;

	protected Number progressValue;

	protected Collection<ReasonerListener> reasonerListeners = new LinkedList<ReasonerListener>();

	public AbstractReasoner() {
	}

	public void setStoreGivenLinks(boolean storeGivenLinks) {
		this.storeGivenLinks = storeGivenLinks;
	}

	/**
	 * populate impliedLinkDatabase from  linkDatabase
	 */
	protected abstract void doReasoning();

	protected abstract void doAddLink(Link link);

	protected MutableLinkDatabase createImpliedLinkDatabase(
			LinkDatabase linkDatabase) {
		// linkDatabase argument ignored? not overridden in LPR... [cjm]
		return new DefaultMutableLinkDatabase(true);
	}

	public void cancel() {
		cancelled = true;
	}

	public Collection<Explanation> getExplanations(PathCapable link) {
		Collection<Explanation> exps = explanationMap.get(link);
		if (exps.size() == 0 && !storeGivenLinks && link instanceof Link) {
			return Collections.singleton((Explanation) new GivenExplanation(
					(Link) link));
		} else
			return exps;
	}

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	public Set<LinkedObject> getParentsOfType(LinkedObject a, OBOProperty b) {
		Set<LinkedObject> out = new LinkedHashSet<LinkedObject>();
		Iterator<Link> it = getParents(a).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isSubPropertyOf(link.getType(), b)) {
				out.add(link.getParent());
			}
		}
		return out;
	}

	public Link hasRelationship(LinkedObject a, OBOProperty b, LinkedObject c) {
		Iterator<Link> it = getParents(a).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (TermUtil.isIntersection(link))
				continue;
			if (isSubPropertyOf(link.getType(), b)
					&& link.getParent().equals(c)) {
				return link;
			}
		}
		return null;
	}

	public boolean isCancelled() {
		return cancelled;
	}

	public boolean isRunning() {
		return running;
	}

	public boolean isSubPropertyOf(OBOProperty a, OBOProperty b) {
		return isSubclass(a, b);
	}

	public boolean isSubclassOf(OBOClass a, OBOClass b) {
		return isSubclass((LinkedObject) a, (LinkedObject) b);
	}

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

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}

	public Collection<IdentifiedObject> getObjects() {
		return linkDatabase.getObjects();
	}

	public Collection<Link> getChildren(LinkedObject lo) {
		if (!storeGivenLinks) {
			Collection<Link> given = linkDatabase.getChildren(lo);
			Collection<Link> impliedChildren = impliedLinkDatabase
					.getChildren(lo);
			Collection<Link> out = new LinkedHashSet<Link>(given.size()
					+ impliedChildren.size());
			out.addAll(given);
			out.addAll(impliedChildren);
			return out;
		} else
			return impliedLinkDatabase.getChildren(lo);
	}

	public Collection<Link> getParents(LinkedObject lo) {
		if (!storeGivenLinks) {
			Collection<Link> given = linkDatabase.getParents(lo);
			Collection<Link> impliedParents = impliedLinkDatabase
					.getParents(lo);
			Collection<Link> out = new LinkedHashSet<Link>(given.size()
					+ impliedParents.size());
			out.addAll(given);
			out.addAll(impliedParents);
			return out;
		} else
			return impliedLinkDatabase.getParents(lo);
	}

	public IdentifiedObject getObject(String id) {
		return linkDatabase.getObject(id);
	}

	public boolean isInstanceOf(Instance a, OBOClass b) {
		return isSubclassOf((OBOClass) a.getType(), b);
	}

	public boolean isRedundant(Link link) {
		return ReasonerUtil.isRedundant(this, link);
	}

	/**
	 *  called before doReasoner() on recache() --
	 *   resets impliedLinkDatabase, explanationMap, explanationDeps
	 */
	protected void initReasoner() {
		running = true;
		fireStart();
		
		// argument appears to be ignored -- CJM
		impliedLinkDatabase = createImpliedLinkDatabase(getLinkDatabase());
		explanationMap =
			new MultiHashSetMap<Link, Explanation>();
		explanationDeps =
			new MultiHashSetMap<Link, Explanation>();
	}

	protected void cleanupReasoner() {
		fireDone();
		running = false;
	}

	public void addLink(Link link) {
		running = true;
		doAddLink(link);
		running = false;
	}

	public void removeLink(Link link) {
		running = true;
		doRemoveLink(link);
		running = false;
	}

	protected void doRemoveLink(Link link) {
		reasonRemoval(link);
	}

	protected void reasonRemoval(Link link) {
		// actually remove the dead link from the various caches
		impliedLinkDatabase.removeParent(link);
		explanationMap.remove(link);

		// find all the explanations that depend on the dead link
		Collection<Explanation> deps = explanationDeps.get(link);
		if (deps == null)
			return;
		for (Explanation exp : deps) {
			// remove the now-defunct link as supporting evidence for the
			// dependent
			// explanation
			boolean dead = exp.removeEvidence(link);
			// if dead == true, it means that removing the defunct link
			// invalidated
			// the explanation, so that explanation needs to be removed
			if (dead) {
				// ditch the explanation
				
				// Since we're removing this explanation, this explanation no longer
				// relies
				// on any supporting links, so it needs to be removed from
				// explanationDeps.
				// Therefore, iterate through all the other links that support this
				// evidence
				// For each of those links, remove the now-defunct explanation from the
				// list
				// of explanations that relies on that link
				for (Link ev : exp.getEvidence()) {
					Collection<Explanation> exps = explanationDeps.get(ev);
					if (exps != null) {
						exps.remove(exp);
						if (exps.size() == 0)
							explanationDeps.remove(ev);
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
		}
	}

	protected void reasonRemoval(PathCapable pc) {
		if (pc instanceof Link)
			reasonRemoval((Link) pc);
	}

	protected static boolean isGiven(Explanation exp) {
		return exp.getExplanationType().equals(ExplanationType.GIVEN);
	}

	/**
	 * Add a link to the impliedLinkDatabase, and also cache the
	 * dependency between the link and its explanation (if not given)
	 * @param link
	 * @param explanation
	 */
	protected void explain(Link link, Explanation explanation) {
		if (storeGivenLinks || !isGiven(explanation)) {
			internalAddLink(link);
			internalAddExplanation(link, explanation);
		}
	}
	
	long expTime = 0;
	protected void internalAddExplanation(Link link, Explanation explanation) {
		long time = System.nanoTime();
		explanationMap.add(link, explanation);
		for (Link evidence : explanation.getEvidence()) {
			explanationDeps.add(evidence, explanation);
		}
		expTime += System.nanoTime() - time;
	}

	protected void internalAddLink(Link link) {
		impliedLinkDatabase.addParent(link);
	}

	/* (non-Javadoc)
	 * @see org.obo.reasoner.ReasonedLinkDatabase#recache()
	 */
	public long recache() {
		long time = System.currentTimeMillis();

		initReasoner();
		doReasoning();
		// createDepMap();
		cleanupReasoner();
		return System.currentTimeMillis() - time;
	}

	public static MultiMap<Link, Explanation> createDepMap(
			MultiMap<Link, Explanation> expMap) {
		MultiMap<Link, Explanation> out = new MultiHashMap<Link, Explanation>();
		for (Explanation exp : expMap.singleValues()) {
			for (Link evidence : exp.getEvidence()) {
				out.add(evidence, exp);
			}
		}
		return out;
	}

	public String getProgressString() {
		return progressString;
	}

	protected void setProgressString(String progressString) {
		this.progressString = progressString;
	}

	protected void setProgressValue(Number progressValue) {
		this.progressValue = progressValue;
	}

	public Number getProgressValue() {
		return progressValue;
	}

}
