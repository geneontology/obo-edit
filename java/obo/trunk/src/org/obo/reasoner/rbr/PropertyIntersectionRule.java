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
import org.obo.util.TermUtil;

import org.apache.log4j.*;

/**
 * X RI Y <- RI=R1^R2^R3... & X R1 Y & X R2 Y & ...
 * 
 * Example. Given:
 * <code>
 * [Typedef]
 * id: ends_before_end_of
 * name: ends_before_end_of
 * 
 * [Typedef]
 * id: starts_after_start_of
 * name: starts_after_start_of
 * 
 * [Typedef]
 * id: during
 * name: during
 * intersection_of: ends_before_end_of
 * intersection_of: starts_after_start_of
 * 
 * [Term]
 * id: gastrulation
 * relationship: starts_after_start_of lifecycle
 * 
 * [Term]
 * id: gastrulation
 * relationship: ends_before_end_of lifecycle
 * 
 * [Term]
 * id: lifecycle
 * 
 * </code>
 * 
 * We can infer that gastrulation during lifecycle
 * 
 * @author cjm
 * 
 */
public class PropertyIntersectionRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PropertyIntersectionRule.class);

	/**
	 * For example:
	 * p^q -> [ p, q]
	 */
	protected MultiMap<OBOProperty, OBOProperty> propertyIntersectionMap;
	
	/**
	 * reverse map
	 */
	protected MultiMap<OBOProperty, OBOProperty> hintMap;
	protected MultiMap<OBOProperty, OBOProperty> piMap;

	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		buildPropertyIntersectionMap(reasoner);
	}

	protected void buildPropertyIntersectionMap(ReasonedLinkDatabase reasoner) {
		propertyIntersectionMap = new MultiHashMap<OBOProperty, OBOProperty>();
		hintMap = new MultiHashMap<OBOProperty, OBOProperty>();
		piMap = new MultiHashMap<OBOProperty, OBOProperty>();
		
		for (IdentifiedObject io : reasoner.getObjects()) {
			if (io instanceof OBOProperty) {
				OBOProperty p = (OBOProperty) io;
				for (Link link : p.getParents()) {
					if (TermUtil.isIntersection(link)) {
						propertyIntersectionMap.add(p, (OBOProperty) link.getParent());
						hintMap.add((OBOProperty) link.getParent(), p);
						piMap.add(p, (OBOProperty) link.getParent());
					}
				}
			}
		}
	}
	
	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		if (piMap == null) {
			// TODO - automatically turn off this rule if not required
			return expls;
		}
		for (OBOProperty inferredProp : piMap.keySet()) {
			// find the xp link that has the fewest match links and
			// use this to build candidate list (this will be
			// the most efficient)
			Integer minSize = null;
			OBOProperty bestProp = null;
			for (OBOProperty iProp : piMap.get(inferredProp)) {
				int numLinks = reasoner.getLinks(iProp).size();
				if (minSize == null || numLinks < minSize) {
					minSize = numLinks;
					bestProp = iProp;
				}
			}
			ArrayList<Link> additionalSupportingLinks = new ArrayList<Link>();
			for (Link candidateSupportingLink : reasoner.getLinks(bestProp)) {
				LinkedObject su = candidateSupportingLink.getChild();
				LinkedObject ob = candidateSupportingLink.getParent();
				boolean isMatch = true;
				// on the first pass we collect potential candidates
				for (OBOProperty iProp : piMap.get(inferredProp)) {
					if (iProp.equals(bestProp))
						continue;
					if (!reasoner.getParentsOfType(su, iProp).contains(ob)) {
						isMatch = false;
						break;
					}
					additionalSupportingLinks.add(createLink(su, iProp, ob));
				}
				if (isMatch) {
					Link out = createLink(su, inferredProp, candidateSupportingLink.getParent());
					PropertyIntersectionExplanation exp;
					exp = new PropertyIntersectionExplanation(); 
					exp.setExplainedLink(out);
					exp.addEvidence(candidateSupportingLink);
					for (Link sl: additionalSupportingLinks)
						exp.addEvidence(sl);
					expls.add(exp);
				}
			}
		}
		return expls;	
	}



}
