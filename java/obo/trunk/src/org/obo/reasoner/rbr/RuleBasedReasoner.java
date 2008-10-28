package org.obo.reasoner.rbr;

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

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.impl.AbstractLinkDatabase;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerListener;
import org.obo.reasoner.impl.AbstractReasoner;
import org.obo.reasoner.impl.AbstractReasonerRule;
import org.obo.reasoner.impl.GivenExplanation;
import org.obo.reasoner.impl.ReasonerRule;
import org.obo.util.TermUtil;

import org.apache.log4j.*;
import org.bbop.util.MultiHashSetMap;

/**
 *
 */
//public class RuleBasedReasoner extends AbstractLinkDatabase implements ReasonedLinkDatabase  {
public class RuleBasedReasoner extends AbstractReasoner {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RuleBasedReasoner.class);

	/**
	 * the LPR works by maintaining a linkPile. initialized on the doReasoning() step,
	 * 
	 */
	protected List<Rule> rules = new ArrayList<Rule>();
	protected int lastVal;
	protected HashMap<Link, Link> linkMap;
	protected HashMap<OBOProperty,Link> propertyLinkMap;
	protected RelationCompositionTable rct;

	/**
	 * a Link that has been inferred by the LinkPileReasoner.
	 *
	 * A triple child-type-parent; immutable, for faster hashing
	 */
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

	public RuleBasedReasoner() {
		//setStoreGivenLinks(false);
		addDefaultRules();
	}

	protected void addDefaultRules() {
		// SymmetryRule is weakly deprecated - we rarely have type-level symmetric relations
		// addRule(new SymmetryRule());
		addRule(new SubPropertyRule());
		//addRule(new PropagateOverIsARule());
		//addRule(new TransitiveRelationRule());
		//addRule(new TransitiveOverRule());
		addRule(new LinkCompositionRule());
		addRule(new PropertyIntersectionRule());
		addRule(new IntersectionRule());

	}
	
	// TODO: 
	public boolean hasDelayedIncrementalMode() {
		return true;
	}
	
	


	public RelationCompositionTable getRelationCompositionTable() {
		return rct;
	}

	/**
	 *  called before doReasoner() on recache() --
	 *   resets impliedLinkDatabase, explanationMap, explanationDeps
	 */
	@Override
	protected void initReasoner() {
		running = true;
		fireStart();

		impliedLinkDatabase = createImpliedLinkDatabase(getLinkDatabase());
		rct = new RelationCompositionTable(getLinkDatabase());
		logger.info("RCT:\n"+rct.toTable());
		explanationMap =
			new MultiHashSetMap<Link, Explanation>();
		explanationDeps =
			new MultiHashSetMap<Link, Explanation>();
		// we seed the impliedLinkDatabase with all given links
		Iterator<Link> it = TermUtil.getAllLinks(linkDatabase);
		while (it.hasNext()) {
			Link link = it.next();
			Explanation e = new GivenExplanation(link);
			addExplanation(e); // adds the link too
		}

	}

	// called from ReasonerOperationModel. 
	// doReasoning() is protected so we need to go via this public method.
	// TODO: need a cleaner way of reasoners genericaly exposing this method
	public void findNewImplications() {
		doReasoning();
	}


	//	@Override
	protected void doReasoning() {

		long initTime = System.nanoTime();

		for (Rule rule : rules) {
			rule.init(this);
		}
		setProgressString("Initializing reasoner...");

		setProgressString("Reasoning...");
		boolean isExhausted = false;
		int sweep = 0;
		int newLinks = 0;
		while (!isExhausted) {
			sweep++;
			logger.info("sweep: "+sweep);
			isExhausted = true;
			for (Rule rule : rules) {
				logger.info("  rule: "+rule);
				Collection<Explanation> expls = rule.getNewInferences(this);
				if (expls == null)
					continue;
				for (Explanation expl : expls) {
					Link link = (Link) expl.getExplainedObject();
					if (link.getChild().equals(link.getParent())) {
						// no reflexive links
					}
					else {
						Link existingLink = hasRelationship(link.getChild(), link.getType(), link.getParent());
						/*
						if (existingLink != null &&

								expl.getExplanationType() != ExplanationType.GENUS &&
								expl.getExplanationType() != ExplanationType.DIFFERENTIA) {
							// do not add redundant explanation
							// - the exception is genus and differentia "trivial" inferences
							// both should be present
						}
						 */
						if (false && existingLink != null && rule.isRedundant(this, existingLink)) {
						}
						else {
							//logger.debug("  new link: "+link);
							if (addExplanation(expl)) {
								newLinks++;
								isExhausted = false;
							}
						}
					}
				}
				logger.info("  new links: "+newLinks);

			}
		}
		logger.info("finished on sweep: "+sweep);
		logger.info("new links: "+newLinks);
		for (Rule rule : rules) {
			rule.end(this);
		}
		long totalTime = System.nanoTime() - initTime;
		logger.info("   Total reasoner time = "
				+ (totalTime / 1000000d) + " ms");

		for (Rule rule : rules) {
			if (rule instanceof AbstractRule)
				logger.info("   time in rule (" + rule + ") = "
						+ (((AbstractRule) rule).ruleTime / 1000000d)
						+ " ms");

		}
	}

	public void addRule(Rule rule) {
		rules.add(rule);
		rule.install(this);
	}

	public void removeRule(Rule rule) {
		rules.remove(rule);
		rule.uninstall(this);
	}



	protected boolean addExplanation(Explanation explanation) {
		Link link = (Link) explanation.getExplainedObject();

		// add link to impliedLinkDatabase, and attach explanation to link
		return addLinkWithExplanation(link, explanation);
	}

	// this replaces explain in Abstract
	private boolean addLinkWithExplanation(Link link, Explanation explanation) {
		//logger.debug("adding link "+link+" ;; expl: "+explanation);
		internalAddLink(link);
		long time = System.nanoTime();
		Collection<Explanation> existingExpls = getExplanations(link);
		if (existingExpls.size() > 1) {
			return false; // never need >1 explanation
		}
		if (existingExpls.size() == 1) {
			if (!existingExpls.iterator().next().getExplanationType().equals(ExplanationType.GIVEN)) {
				return false; // there is at least one reasoner explanation
			}
		}
		explanationMap.add(link, explanation);
		//logger.debug("added to explMap: "+link+" -> " +explanation);
		for (Link evidence : explanation.getEvidence()) {
			explanationDeps.add(evidence, explanation);
		}
		expTime += System.nanoTime() - time;
		return true;
	}


	@Override
	public Collection<Explanation> getExplanations(PathCapable link) {
		Collection<Explanation> exps = explanationMap.get(link);
		return exps;
	}


	// incremental reasoning
	protected void doAddLink(Link link) {
		addExplanation(new GivenExplanation(link));
		doReasoning(); // TODO - selective reasoning
	}
	public Collection<Link> getChildren(LinkedObject lo) {
		return impliedLinkDatabase.getChildren(lo);
	}

	public Collection<Link> getParents(LinkedObject lo) {
		return impliedLinkDatabase.getParents(lo);
	}

	protected void doRemoveLink(Link link) {
		cascadingRemoveLink(link);
		//doReasoning(); // it may be possible to re-populate some links via different explanations
		//doReasoning(); // TODO - don't need to add all givens every time.. CHANGED - but in an ugly way. ReasonerOperationModel takes care of this at the end of a macro op
	}

	protected void cascadingRemoveLink(Link link) {
		logger.info("removing link:"+link);
		// actually remove the dead link from the various caches
		impliedLinkDatabase.removeParent(link);
		explanationMap.remove(link);

		// find all the explanations that depend on the dead link
		Collection<Explanation> deps = explanationDeps.get(link);
		if (deps == null)
			return;
		Collection<Link> togo = new HashSet<Link>();
		for (Explanation exp : deps) {
			Link depLink = (Link) exp.getExplainedObject();
			logger.debug("link : "+link+" // has DEP: "+depLink+" // via EXPL: "+exp);
			togo.add(depLink);
		}
		explanationDeps.remove(link);
		for (Link depLink : togo) {
			cascadingRemoveLink(depLink);
		}
	}

	@Override
	public Set<LinkedObject> getParentsOfType(LinkedObject a,
			OBOProperty prop) {
		Set<LinkedObject> out = new LinkedHashSet<LinkedObject>();
		Iterator<Link> it = getParents(a).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (link.getType().equals(prop)) {
				out.add(link.getParent());
			}
		}
		return out;
	}

	// TODO: use an index
	public Link hasRelationship(LinkedObject a, OBOProperty prop, LinkedObject b) {
		Iterator<Link> it = getParents(a).iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (link.getParent().equals(b) && link.getType().equals(prop)) {
				return link;
			}
		}
		return null;
	}


	public boolean isInstanceOf(Instance a, OBOClass b) {
		// TODO Auto-generated method stub
		return false;
	}


	public boolean isRunning() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isSubPropertyOf(OBOProperty a, OBOProperty b) {
		return hasRelationship(a, OBOProperty.IS_A, b) != null;
	}

	public boolean isSubclassOf(OBOClass a, OBOClass b) {
		return hasRelationship(a, OBOProperty.IS_A, b) != null;
	}




}
