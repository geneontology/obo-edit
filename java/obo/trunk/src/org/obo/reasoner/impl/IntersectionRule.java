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

public class IntersectionRule extends AbstractReasonerRule {

	protected MultiMap<LinkedObject, Link> intersectionMap;
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
		Collection<LinkedObject> candidates = hintMap.get(newLink.getParent());
		Collection<Explanation> out = new ArrayList<Explanation>(candidates
				.size());
		Collection<CompletenessMatch> matchList =
			new LinkedList<CompletenessMatch>();
		for (LinkedObject lo : candidates) {
			if (lo.equals(newLink.getChild()))
				continue;
			boolean failed = false;
			matchList.clear();
			Collection<Link> intersectionLinks = intersectionMap.get(lo);
			for (Link link : intersectionLinks) {
				Link matchLink = reasoner.hasRelationship(newLink.getChild(),
						link.getType(), link.getParent());				
				if (matchLink == null) {
					failed = true;
					break;
				} else {
					matchList.add(new CompletenessMatch(matchLink, link));
				}
			}
			if (!failed) {
				Link genLink = createLink(newLink
						.getChild(), OBOProperty.IS_A, lo);
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
