package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.CompletenessExplanation;
import org.obo.reasoner.impl.CompletenessMatch;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

/**
 * 
 * [xp2] For any class X & Y, if Y has a cross-product definition consisting of
 * link elements E1, E2, ... En
 * <p>
 * <code>
 * forall Ei in E :
 *  IF (Ei is a class and X is_a Ei)
 *     Or (Ei=<R,Z> and X R Z)
 *  THEN X is_a Y
 * </code>
 * 
 * Example:
 * <code>
 *  T-cell differentiation = cell differentiation THAT
 * results_in_acquisition_of_features_of T-cell
 * 
 * lymphocyte differentiation =
 * cell differentiation THAT results_in_acquisition_of_features_of lymphocyte
 * 
 * T-cell is_a lymphocyte 
 * 
 * =>
 * 
 * T-cell differentiation
 * results_in_acquisition_of_features_of T-cell [xp1] lymphocyte differentiation
 * results_in_acquisition_of_features_of lymphocyte [xp1] T-cell differentiation
 * results_in_acquisition_of_features_of lymphocyte [propagation over is_a]
 * T-cell differentiation is_a cell differentiation [xp1] => T-cell
 * differentiation is_a lymphocyte differentiation [xp2]
 * </code>
 * 
 * @author cjm
 * 
 */
public class IntersectionRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionRule.class);
	protected boolean isFirstPass = true;
	public long findBestLinkTime = 0;

	/**
	 * maps from the OBOClass which is defined to the links comprising the intersection definition.
	 * For example:
	 * T cell differentiation -> [ <is_a differentiation>, <results_in_acquisition_of_features_of T-cell>]
	 */
	protected MultiMap<LinkedObject, Link> intersectionMap;
	
	
	/**
	 * maps from a defined OBOClass to the most informative link comprising
	 * part of the set of xp links; this is the link that has fewest children.
	 * this is used for optimization purposes, the actual link chosen should
	 * not affect the final outcome.
	 * For example, if blue_car is defined using the two links
	 * [1] is_a car
	 * [2] has_color blue
	 * 
	 * and there are many more blue colored things than cars, then [1] is chosen
	 * as the best link
	 */
	protected Map<LinkedObject, Link> bestLinkMap;


	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		buildIntersectionMap(reasoner);
	}

	protected void buildIntersectionMap(ReasonedLinkDatabase reasoner) {
		intersectionMap = new MultiHashMap<LinkedObject, Link>();
		Collection<LinkedObject> skipset = new HashSet<LinkedObject>();
		for (IdentifiedObject io : reasoner.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				for (Link link : lo.getParents()) {
					if (TermUtil.isIntersection(link)) {
						intersectionMap.add(lo, link);
						OBORestriction r = (OBORestriction) link;
						if (r.getCardinality() != null ||
								(r.getMinCardinality() != null &&
										r.getMinCardinality() != 1) ||
										r.getMaxCardinality() != null) {
							logger.info("Ignoring xp def of "+lo+" Reason: cardinality ");
							skipset.add(lo);
						}
						if (r.getAdditionalArguments() != null &&
								r.getAdditionalArguments().size() > 0) {
							logger.info("Ignoring xp def of "+lo+" Reason: n-ary relations ");
							skipset.add(lo);
						}
					}
				}
			}
			for (LinkedObject s : skipset) {
				intersectionMap.remove(s);
			}
		}
	}

	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		long time;
		time = System.nanoTime();

		ArrayList<Explanation> expls = new ArrayList<Explanation>();

		// add trivial links
		if (isFirstPass) {
			long itime = System.nanoTime();
			bestLinkMap = new HashMap<LinkedObject,Link>();

			// TODO!! do we need this? oe expects impliedLinkDatabase to contain both..
			isFirstPass = false;
			for (LinkedObject xp : intersectionMap.keySet()) {
				// find the xp link that has the fewest match links and
				// use this to build candidate list (this will be
				// the most efficient)
				Integer minSize = null;
				Link bestLink = null;
				for (Link link : intersectionMap.get(xp)) {

					OBOProperty prop = link.getType();
					Link out = createLink(link.getChild(), prop, link.getParent());
					AbstractExplanation exp;
					if (prop.equals(OBOProperty.IS_A))
						exp = new GenusExplanation(link);
					else
						exp = new DifferentiaExplanation(link);
					exp.setExplainedLink(out);
					expls.add(exp);

					int numLinks = reasoner.getChildren(link.getParent()).size(); // we map by properties so this should be fast
					if (minSize == null || numLinks < minSize) {
						minSize = numLinks;
						bestLink = link;
					}
				}
				bestLinkMap.put(xp, bestLink);
			}
			long xpInitTime = System.nanoTime() - itime;
			logger.info("      xpInitTime = "
					+ (xpInitTime / 1000000d) + " ms");
		}
		
		// trivial links are added; now the main classification step
		for (LinkedObject xp : intersectionMap.keySet()) {

			// each candidate is a potential subclass of xp
			Set<LinkedObject> candidateSubClasses = new HashSet<LinkedObject>();

			// choose the optimal link to test first; the choice only
			// affects performance, not results
			Link bestLink = null;
			if (bestLinkMap.containsKey(xp))
				bestLink = bestLinkMap.get(xp);
			else
				// choose an arbitrary candidate link
				// could be inefficient for ontologies e.g. with lots of genus-root terms
				bestLink = intersectionMap.get(xp).iterator().next(); // arbitrary


			// find candidates based on one of the N+S conditions
			Collection<Link> rchildren = reasoner.getChildren(bestLink.getParent());
			Link reflexiveLink = null;
			if (bestLink.getType().equals(OBOProperty.IS_A) || bestLink.getType().isReflexive()) {
				// add reflexive link.
				// reasoner.getChildren() does NOT return a clone.
				// we want to TEMPORARILY add the relfexive link
				reflexiveLink = 
					new OBORestrictionImpl(bestLink.getParent(),
							bestLink.getParent(), bestLink.getType());
				rchildren.add(reflexiveLink);
			}
			for (Link candidateLink : rchildren) {
				OBOProperty prop = candidateLink.getType();
				if (prop.equals(bestLink.getType())) {
					LinkedObject candidateSubClass = candidateLink.getChild();
					Link existingLink = reasoner.hasRelationship(candidateSubClass, prop, xp);
					if (existingLink != null) {
						Collection<Explanation> existingExpls = reasoner.getExplanations(existingLink);
						if (!onlyGiven(existingExpls))
							continue; // we have this already
					}	

					// on the first pass we collect potential candidates
					boolean satisfies = true;
					for (Link nsLink : intersectionMap.get(xp)) {
						if (nsLink.equals(bestLink))
							continue;
						if (reasoner.hasRelationship(candidateSubClass, nsLink.getType(), nsLink.getParent()) == null) {
							satisfies = false;
							break;
						}
					}
					if (satisfies)
						candidateSubClasses.add(candidateSubClass);
				}
			}
			// reasoner.getChildren() does NOT return a clone
			if (reflexiveLink != null)
				rchildren.remove(reflexiveLink);

			for (LinkedObject candidate : candidateSubClasses) {
				Link out = createLink(candidate, OBOProperty.IS_A, xp);
				CompletenessExplanation exp;
				exp = new CompletenessExplanation(); 
				exp.setExplainedLink(out);
				for (Link nsLink : intersectionMap.get(xp)) {
					Link matchLink = createLink(candidate, nsLink.getType(), nsLink.getParent());
					//IntersectionMatch m = new IntersectionMatch(matchLink, nsLink); 
					CompletenessMatch m = new CompletenessMatch(matchLink, nsLink); 
					exp.addMatch(m);
				}
				expls.add(exp);	
			}
		}
		logger.info("      findBestLinkTime = "
				+ (findBestLinkTime / 1000000d) + " ms");

		ruleTime += (System.nanoTime() - time);
		return expls;	
	}


}
