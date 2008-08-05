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
public class PropertyIntersectionRule extends AbstractReasonerRule {

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

	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		buildPropertyIntersectionMap(reasoner);
	}

	protected void buildPropertyIntersectionMap(ReasonedLinkDatabase reasoner) {
		propertyIntersectionMap = new MultiHashMap<OBOProperty, OBOProperty>();
		hintMap = new MultiHashMap<OBOProperty, OBOProperty>();
		
		for (IdentifiedObject io : reasoner.getObjects()) {
			if (io instanceof OBOProperty) {
				OBOProperty p = (OBOProperty) io;
				for (Link link : p.getParents()) {
					if (TermUtil.isIntersection(link)) {
						propertyIntersectionMap.add(p, (OBOProperty) link.getParent());
						hintMap.add((OBOProperty) link.getParent(), p);
					}
				}
			}
		}
	}

	@Override
	protected Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
		
		Collection<OBOProperty> candidateProperties = hintMap.get(newLink.getType());
			
		Collection<Explanation> out = new ArrayList<Explanation>(1);
		if (candidateProperties.size() == 0)
			return out;
		
		LinkedObject newLinkChild = newLink.getChild();
		LinkedObject newLinkParent = newLink.getParent();

		for (OBOProperty p : candidateProperties) {
			Collection<Link> supportingLinks = new ArrayList<Link>(candidateProperties.size());
			boolean failed = false;
			Collection<OBOProperty> propertyIntersectionElements = propertyIntersectionMap.get(p);
			for (OBOProperty pie : propertyIntersectionElements) {
				Link matchLink = reasoner.hasRelationship(newLinkChild,
						pie, newLinkParent);				
				if (matchLink == null) {
					failed = true;
					break;
				} else {
					supportingLinks.add(matchLink);
				}
			}
			if (!failed) {
				/*
				 * the necessary and sufficient conditions have been satisfied.
				 */
				Link genLink = createLink(newLinkChild, p, newLinkParent);
				
				/*
				 * explain this link
				 */
				PropertyIntersectionExplanation exp = new PropertyIntersectionExplanation();
				exp.setExplainedLink(genLink);
				for (Link supportingLink : supportingLinks)
					exp.addEvidence(supportingLink);
				out.add(exp);
			}
		}
		return out;
	}

}
