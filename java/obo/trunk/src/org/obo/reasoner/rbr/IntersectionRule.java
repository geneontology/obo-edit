package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
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

	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		long time;
		time = System.nanoTime();

		ArrayList<Explanation> expls = new ArrayList<Explanation>();

		if (isFirstPass) {
			// TODO!! do we need this? oe expects impliedLinkDatabase to contain both..
			isFirstPass = false;
			for (LinkedObject xp : intersectionMap.keySet()) {
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

				}
			}
		}
		for (LinkedObject xp : intersectionMap.keySet()) {

			// each candidate is a potential subclass of xp
			Set<LinkedObject> candidateSubClasses = new HashSet<LinkedObject>();

			// find the xp link that has the fewest match links and
			// use this to build candidate list (this will be
			// the most efficient)
			Integer minSize = null;
			Link bestLink = null;
			for (Link nsLink : intersectionMap.get(xp)) {
				OBOProperty p = nsLink.getType();
				int numLinks = reasoner.getLinks(p).size();
				if (minSize == null || numLinks < minSize) {
					minSize = numLinks;
					bestLink = nsLink;
				}
			}
			for (Link candidateLink : reasoner.getChildren(bestLink.getParent())) {
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
					candidateSubClasses.add(candidateSubClass);
				}
			}
			for (Link nsLink : intersectionMap.get(xp)) {
				if (nsLink.equals(bestLink))
					continue;
				OBOProperty prop = nsLink.getType();
				LinkedObject to = nsLink.getParent();
				Collection<LinkedObject> togo = new ArrayList<LinkedObject>();
				for (LinkedObject candidate : candidateSubClasses) {
					if (reasoner.hasRelationship(candidate, prop, to) == null) {
						togo.add(candidate);
					}
				}
				candidateSubClasses.removeAll(togo);
			}

			for (LinkedObject candidate : candidateSubClasses) {
				Link out = createLink(candidate, OBOProperty.IS_A, xp);
				IntersectionExplanation exp;
				exp = new IntersectionExplanation(); // TODO
				exp.setExplainedLink(out);
				for (Link nsLink : intersectionMap.get(xp)) {
					Link matchLink = createLink(candidate, nsLink.getType(), nsLink.getParent());
					IntersectionMatch m = new IntersectionMatch(matchLink, nsLink); // TODO
					exp.addMatch(m);
				}
				expls.add(exp);	
			}
		}
		ruleTime += (System.nanoTime() - time);
		return expls;	
	}


}
