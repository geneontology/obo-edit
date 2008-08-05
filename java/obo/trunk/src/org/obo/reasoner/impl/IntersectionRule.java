package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
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
public class IntersectionRule extends AbstractReasonerRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionRule.class);

	/**
	 * maps from the OBOClass which is defined to the links comprising the intersection definition.
	 * For example:
	 * T cell differentiation -> [ <is_a differentiation>, <results_in_acquisition_of_features_of T-cell>]
	 */
	protected MultiMap<LinkedObject, Link> intersectionMap;
	
	/**
	 * maps from an OBOClass used in an intersection definition to the defined class. For example
	 * T-cell -> [T-cell differentiation, T-cell proliferation, T-cell anergy, ...]
	 */
	protected MultiMap<LinkedObject, LinkedObject> hintMap;

	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		buildIntersectionMap(reasoner);
	}

	protected void buildIntersectionMap(ReasonedLinkDatabase reasoner) {
		intersectionMap = new MultiHashMap<LinkedObject, Link>();
		hintMap = new MultiHashMap<LinkedObject, LinkedObject>();
		for (IdentifiedObject io : reasoner.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				for (Link link : lo.getParents()) {
					if (TermUtil.isIntersection(link)) {
						intersectionMap.add(lo, link);
						hintMap.add(link.getParent(), lo);
					}
				}
			}
		}
	}

	@Override
	protected Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
		
		/*
		 * this rule infers is_a (subsumption) links to some candidate parent(s).
		 * It is triggered by the addition of a link that satisfies *one* of the N+S conditions of the xp
		 * def of the parent. If *all* if the N+S conditions are satisfied then a new link is added
		 * 
		 * example:
		 *   newLink =  <T-cell-prolif results_in_proliferation_of lymphocyte>
		 *     (in this case newLink has already been inferred via transitivity over is_a)
		 *   candidates = [lymphocyte-prolif, ...]
		 *   inferred link = <T-cell-prolif is_a lymphocyte-prolif>
		 */
		Collection<LinkedObject> candidates = hintMap.get(newLink.getParent());
		Collection<Explanation> out = new ArrayList<Explanation>(candidates.size());
		
		/*
		 * potential new links to add
		 * example:
		 *   T-cell-prolif is_a lymphocyte prolif
		 */
		Collection<CompletenessMatch> matchList =
			new LinkedList<CompletenessMatch>();
		
		/*
		 * Example: T-cell-prolif
		 */
		LinkedObject newLinkChild = newLink.getChild();
		
		/*
		 * each candidate parent must have its necessary and sufficient conditions (xp def) satisfied
		 */
		for (LinkedObject lo : candidates) {
			if (lo.equals(newLinkChild))
				continue;
			boolean failed = false;
			matchList.clear();
			Collection<Link> intersectionLinks = intersectionMap.get(lo);
			for (Link link : intersectionLinks) {
				Link matchLink = reasoner.hasRelationship(newLinkChild,
						link.getType(), link.getParent());				
				if (matchLink == null) {
					failed = true;
					break;
				} else {
					matchList.add(new CompletenessMatch(matchLink, link));
				}
			}
			if (!failed) {
				/*
				 * the necessary and sufficient conditions have been satisfied.
				 * 
				 * we create a new is_a link (genLink) between the child of the original triggering
				 * link and the candidate object
				 * 
				 * Example:
				 *   T-cell-prolif is_a lymphocyte-prolif
				 */
				Link genLink = createLink(newLinkChild, OBOProperty.IS_A, lo);
				
				/*
				 * explain this link
				 */
				CompletenessExplanation exp = new CompletenessExplanation();
				exp.setExplainedLink(genLink);
				for(CompletenessMatch match : matchList) {
					exp.addMatch(match);
				}
				out.add(exp);
			}
		}
		return out;
	}

}
