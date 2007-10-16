package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

import org.obo.datamodel.Link;
import org.obo.reasoner.Explanation;
import org.obo.util.TermUtil;

public class LinkPileReasoner extends AbstractReasoner {

	protected Collection<Link> linkPile;
	protected List<ReasonerRule> rules = new ArrayList<ReasonerRule>();
	protected int maxLinkPileSize = 0;

	public LinkPileReasoner() {
		setStoreGivenLinks(false);
		addDefaultRules();
	}

	protected void addDefaultRules() {
		addRule(new SimpleTransitivityRule());
		addRule(new SymmetryRule());
		addRule(new GenusDifferentiaRule());
		addRule(new IntersectionRule());
	}

	@Override
	public Number getProgressValue() {
		if (maxLinkPileSize == 0)
			return 0;
		int val = 100 * (maxLinkPileSize - linkPile.size()) / maxLinkPileSize;
		if (val >= 100)
			return 99;
		if (val >= 0)
			return val;

		return 0;
	}

	@Override
	protected void doReasoning() {
		for (ReasonerRule rule : rules) {
			rule.init(this);
		}
		setProgressString("Initializing reasoner...");
		 linkPile = new LinkedHashSet<Link>();
		//linkPile = new LinkedList<Link>();
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
		Set<Link> lookedat = new HashSet<Link>();
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
			if (lookedat.contains(link))
				continue;
			else
				lookedat.add(link);
			for (ReasonerRule rule : rules) {
				Collection<Explanation> exps = rule.getImplications(this, link);
				if (exps == null)
					continue;
				for (Explanation e : exps) {
					time = System.nanoTime();
					boolean added = addExplanation(e);
					addTimeTrue += System.nanoTime() - time;
					/*
					 * addCount++; if (added) { addTimeTrue += System.nanoTime() -
					 * time; addTrueCount++; } else addTimeFalse +=
					 * System.nanoTime() - time;
					 */
					if (added) {
						if (e.getExplainedObject() instanceof Link) {
							Link newLink = (Link) e.getExplainedObject();
							// if (!TermUtil.containsLink(this, newLink))
							/*
							 * if (linkPile.contains(newLink)) readdedCount++;
							 * else newaddedCount++;
							 */
							if (!lookedat.contains(newLink)) {
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
		System.err.println("   Add time(true(" + addTrueCount + ")) = "
				+ (addTimeTrue / 1000000d) + " ms");
		System.err.println("   Add time(false(" + (addCount - addTrueCount)
				+ ")) = " + (addTimeFalse / 1000000d) + " ms");
		System.err.println("   Pile pull time: " + (pilePullTime / 1000000d)
				+ " ms");
		System.err.println(" readdedCount = " + readdedCount);
		System.err.println(" newaddedCount = " + newaddedCount);
		for (ReasonerRule rule : rules) {
			if (rule instanceof AbstractReasonerRule)
				System.err.println("   time in rule (" + rule + ") = "
						+ (((AbstractReasonerRule) rule).ruleTime / 1000000d)
						+ " ms");

		}
	}

	protected boolean addExplanation(Explanation explanation) {
		Link link = (Link) explanation.getExplainedObject();
		/*
		 * Collection<Explanation> exps = getExplanations(link); if
		 * ((storeGivenLinks || !isGiven(explanation)) &&
		 * exps.contains(explanation)) return false;
		 */
		explain(link, explanation);
		return true;
	}
}
