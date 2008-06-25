package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.reasoner.Explanation;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class LinkPileReasoner extends AbstractReasoner {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkPileReasoner.class);

	protected Collection<Link> linkPile;
	protected List<ReasonerRule> rules = new ArrayList<ReasonerRule>();
	protected int maxLinkPileSize = 0;
	protected int lastVal;
	protected HashMap<Link, Link> linkMap;
	protected boolean lowMemoryMode = false;

	public static class ReasonerLink implements Link {

		protected LinkedObject child;
		protected LinkedObject parent;
		protected OBOProperty type;
		protected boolean lookedAt;
		// el - changed to HashSet to prevent duplicates
		protected HashSet<AbstractExplanation> explanations;
		protected String id;
		protected int hash;

		public ReasonerLink(LinkedObject child, OBOProperty type,
				LinkedObject parent) {
			this.child = child;
			this.type = type;
			this.parent = parent;
			id = child.getID() + '-' + type.getID() + "->" + parent.getID();
			hash = child.hashCode() + type.hashCode() + parent.hashCode();
		}

		public Collection<AbstractExplanation> getExplanations() {
			if (explanations == null)
				return Collections.emptyList();
			else
				return explanations;
		}

		public Object clone() {
			throw new UnsupportedOperationException();
		}

		public void addExplanation(AbstractExplanation exp) {
			exp.setExplainedLink(this);
			if (explanations == null) {
				explanations = new HashSet<AbstractExplanation>(5);
			}
			explanations.add(exp);
		}

		public void removeExplanation(AbstractExplanation exp) {
			if (explanations != null)
				explanations.remove(exp);
		}

		public boolean isImplied() {
			return true;
		}

		public String getID() {
			return id;
		}

		@Override
		public int hashCode() {
			return hash;
		}
		
		public String toString() {
			return child + " -> "+type+" -> "+parent;
		}

		@Override
		public boolean equals(Object o) {
			if (o == null) {
				return false;
			} else if (o instanceof Link) {
				return TermUtil.equals(this, (Link) o);
			} else
				return false;
		}

		public void setNestedValue(NestedValue nv) {
		}

		public NestedValue getNestedValue() {
			return null;
		}

		public boolean isAnonymous() {
			return false;
		}

		public void setNamespace(Namespace namespace) {
		}

		public Namespace getNamespace() {
			return null;
		}

		public LinkedObject getChild() {
			return child;
		}

		public void setChild(LinkedObject child) {
			throw new UnsupportedOperationException();
		}

		public LinkedObject getParent() {
			return parent;
		}

		public void setParent(LinkedObject parent) {
			throw new UnsupportedOperationException();
		}

		public OBOProperty getType() {
			return type;
		}

		public void setType(OBOProperty type) {
			throw new UnsupportedOperationException();
		}

		public boolean isLookedAt() {
			return lookedAt;
		}

		public void setLookedAt(boolean lookedAt) {
			this.lookedAt = lookedAt;
		}
	}

	public LinkPileReasoner() {
		setStoreGivenLinks(false);
		addDefaultRules();
	}

	protected void addDefaultRules() {
		addRule(new SimpleTransitivityRule());
		addRule(new SymmetryRule());
		addRule(new GenusDifferentiaRule());
		addRule(new IntersectionRule());
		addRule(new TransitiveOverRule());
		addRule(new HoldsOverChainRule());
	}

	@Override
	public Number getProgressValue() {
		int val;
		if (maxLinkPileSize == 0)
			val = 0;
		else {
			val = 100 * (maxLinkPileSize - linkPile.size()) / maxLinkPileSize;
			if (val < lastVal)
				val = lastVal;
			else if (val > 99)
				val = 99;
			else if (val < 0)
				val = 0;
		}
		lastVal = val;
		return val;
	}

	@Override
	protected void doReasoning() {
		for (ReasonerRule rule : rules) {
			rule.init(this);
		}
		setProgressString("Initializing reasoner...");
		linkPile = new LinkedHashSet<Link>();
		// linkPile = new LinkedList<Link>();
		Iterator<Link> it = TermUtil.getAllLinks(linkDatabase);
		while (it.hasNext()) {
			Link link = it.next();
			linkPile.add(link);
			if (storeGivenLinks) {
				Explanation e = new GivenExplanation(link);
				addExplanation(e);
			}
		}
		setProgressString("Reasoning...");
		maxLinkPileSize = linkPile.size();
		siftPile();
		for (ReasonerRule rule : rules) {
			rule.end(this);
		}
	}

	public void addRule(ReasonerRule rule) {
		rules.add(rule);
		rule.install(this);
	}

	public void removeRule(ReasonerRule rule) {
		rules.remove(rule);
		rule.uninstall(this);
	}

	@Override
	protected void doAddLink(Link link) {
		linkPile.add(link);
		if (storeGivenLinks) {
			addExplanation(new GivenExplanation(link));
		}
		siftPile();
	}

	protected Collection<Link> getLinkPile() {
		return linkPile;
	}

	protected Link popLink() {
		if (linkPile instanceof Queue)
			return ((Queue<Link>) linkPile).poll();
		else {
			Iterator<Link> it = linkPile.iterator();
			Link out = it.next();
			it.remove();
			return out;
		}
	}

	protected void siftPile() {
		long time;
		long addTimeFalse = 0;
		long addTimeTrue = 0;
		int addCount = 0;
		int addTrueCount = 0;
		int readdedCount = 0;
		int newaddedCount = 0;
		long pilePullTime = 0;
		while (!linkPile.isEmpty()) {
			if (isCancelled())
				return;
			time = System.nanoTime();
			/*
			 * Iterator<Link> it = linkPile.iterator(); Link link = it.next();
			 * it.remove(); it = null;
			 */
			Link link = popLink();
			pilePullTime += System.nanoTime() - time;
			if (isLookedAt(link))
				continue;
			else
				setLookedAt(link);
			for (ReasonerRule rule : rules) {
				Collection<Explanation> exps = rule.getImplications(this, link);
				if (exps == null)
					continue;
				for (Explanation e : exps) {
					time = System.nanoTime();
					boolean added = addExplanation(e);
					addTimeTrue += System.nanoTime() - time;
					if (added) {
						if (e.getExplainedObject() instanceof Link) {
							Link newLink = (Link) e.getExplainedObject();
							if (!isLookedAt(newLink)) {
								linkPile.add(newLink);
								if (linkPile.size() >= maxLinkPileSize)
									maxLinkPileSize = linkPile.size();
							}
						}
					}
				}
			}
			// setProgressString(linkPile.size() + "");
		}

		logger.info("   Add time(true(" + addTrueCount + ")) = "
				+ (addTimeTrue / 1000000d) + " ms");
		logger.info("   Add time(false(" + (addCount - addTrueCount)
				+ ")) = " + (addTimeFalse / 1000000d) + " ms");
		logger.info("   Pile pull time: " + (pilePullTime / 1000000d)
				+ " ms");
		logger.info(" readdedCount = " + readdedCount);
		logger.info(" newaddedCount = " + newaddedCount);
		for (ReasonerRule rule : rules) {
			if (rule instanceof AbstractReasonerRule)
				logger.info("   time in rule (" + rule + ") = "
						+ (((AbstractReasonerRule) rule).ruleTime / 1000000d)
						+ " ms");

		}
	}

	protected boolean addExplanation(Explanation explanation) {
		Link link = (Link) explanation.getExplainedObject();
		explain(link, explanation);
		return true;
	}

	protected boolean isLookedAt(Link link) {
		if (link instanceof ReasonerLink)
			return ((ReasonerLink) link).isLookedAt();
		else
			return false;
	}

	protected void setLookedAt(Link link) {
		if (link instanceof ReasonerLink) {
			((ReasonerLink) link).setLookedAt(true);
		}
	}

	protected void reasonRemoval(Link link) {
		reasonRemoval(link, new HashSet<Link>());
	}

	protected void reasonRemoval(PathCapable pc, Collection<Link> seenem) {
		if (pc instanceof Link)
			reasonRemoval((Link) pc, seenem);
	}

	protected void reasonRemoval(Link link, Collection<Link> seenem) {
		logger.info("Removing link: "+link);
		if (seenem.contains(link))
			return;
		else
			seenem.add(link);
		// actually remove the dead link from the various caches
		impliedLinkDatabase.removeParent(link);
		// el - gotta make sure to remove the link from linkMap as well (otherwise incremental
		// insertion can break
		linkMap.remove(link);
		// re-generate all the implications of the now-removed link
		Collection<Explanation> deps = new ArrayList<Explanation>();
		for (ReasonerRule rule : rules) {
			Collection<Explanation> temp = rule.getImplications(this, link);
			if (temp != null)
				deps.addAll(temp);
			// el - issues with intersection rule if there is a cycle in the explanation graph.
			// this is a bit of a hack, but gets around it by looking at intersection 
			// implications for all the children of the child of the current link
			if (rule instanceof IntersectionRule) {
				for (Link childLink : getChildren(link.getChild())) {
					Link newLink = new ReasonerLink(childLink.getChild(), childLink.getType(), link.getParent());
					temp = rule.getImplications(this, newLink);
					if (temp != null) {
						deps.addAll(temp);
					}
				}
			}
		}

		for (Explanation exp : deps) {
			// get all the other explanations for the explained object
			Collection<Explanation> exps = getExplanations(exp
					.getExplainedObject());
			// pre-emptively remove this explanation from the set (although we
			// may add
			// it back later, we have to do this now, because once we remove
			// evidence
			// from the explanation we change its identity, and the remove()
			// method
			// won't work right)
			exps.remove(exp);
			// remove the now-defunct link as supporting evidence for the
			// dependent explanation
			boolean dead = exp.removeEvidence(link);
			// if dead == true, it means that removing the defunct link
			// invalidated the explanation
			// el - can't do the "dead" check anymore because of the trick we're using for
			// dealing with cyclic explanations
			//if (dead) {
				if (exps.isEmpty()) {
					reasonRemoval(exp.getExplainedObject(), seenem);
				}
			//} else
				//exps.add(exp);
		}
	}

	@Override
	public Collection<Explanation> getExplanations(PathCapable pc) {
		//return johns_version_of_getExplanations_which_I_dont_understand(pc);
		// This version written by CJM
		Collection<Explanation> expls = new HashSet<Explanation>();
		if (pc instanceof Link) {
			Link link = (Link) pc;
			if (!(link instanceof ReasonerLink)) {
				// help! this is guesswork
				expls.add(new GivenExplanation(
						(Link) link));
			}
			Link rl = findRealLink((Link)link);
			
			if (rl instanceof ReasonerLink) {
				//expls.addAll( ((ReasonerLink) rl).getExplanations() );
				// el - need to return a reference to the existing container, can't return
				// a new one
				return (Collection)((ReasonerLink)rl).getExplanations();
			}
		} 
		return expls;
	}
	
	//@Override
	public Collection<Explanation> johns_version_of_getExplanations_which_I_dont_understand(PathCapable link) {
		if (link instanceof ReasonerLink) {
			ReasonerLink rl = (ReasonerLink) link;
			if (!rl.isLookedAt())
				rl = (ReasonerLink) findRealLink(rl);
			return (Collection) rl.getExplanations();
		} else if (link instanceof Link && !TermUtil.isImplied(link)) {
			return (Collection) Collections.singleton(new GivenExplanation(
					(Link) link));
		} else
			return Collections.emptySet();
	}

	/**
	 * 
	 * @param link
	 * @return
	 */
	// TODO - THIS NEEDS EXPLAINED!! -- CJM
	// I made this public for testing purposes
	public Link findRealLink(Link link) {
		if (lowMemoryMode) {
			return TermUtil.getLink(impliedLinkDatabase, link);
		} else {
			if (linkMap == null)
				linkMap = new HashMap<Link, Link>();
			Link candidate = linkMap.get(link);
			// empirical nodes: candidate seems to be the one with explanations attached
			if (candidate == null)
				linkMap.put(link, link);
			else
				link = candidate;
			return link;
		}
	}

	@Override // BUT WHY OVERRIDE??? --- CJM
	protected void internalAddExplanation(Link link, Explanation explanation) {
		if (link instanceof ReasonerLink
				&& explanation instanceof AbstractExplanation) {
			link = findRealLink(link);
			((ReasonerLink) link)
					.addExplanation((AbstractExplanation) explanation);
		}
	}
}
