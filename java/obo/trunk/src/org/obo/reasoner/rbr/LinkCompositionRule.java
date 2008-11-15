package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

import org.apache.log4j.*;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;

public class LinkCompositionRule extends AbstractRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkCompositionRule.class);

	/**
	 * Given a composition table of rules
	 * R1.R2->IR
	 * Start at an object A
	 *   repeat: find all links L1 to B
	 *     find all links L2 from B
	 *     populate new links, add new links to stack
	 * @param reasoner
	 * @param rct
	 * @return
	 */
	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner) {
		RelationCompositionTable rct = ((RuleBasedReasoner)reasoner).getRelationCompositionTable();
		long time;
		time = System.nanoTime();

		// a p0 b, b p1 c -> a ip c
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		for (IdentifiedObject a : reasoner.getObjects()) {
			if (!(a instanceof LinkedObject)) {
				continue;
			}
			LinkedObject ao = (LinkedObject) a;

			//Collection<LinkedObject> seenObjs = new HashSet<LinkedObject>();
			Collection<Link> seenLinks = new HashSet<Link>();

			Collection<Link> extLinks = new HashSet<Link>();
			for (Link extLink : reasoner.getParents(ao))
				if (rct.hasComposition(extLink.getType()))
					extLinks.add(extLink);
			while (extLinks.size() > 0) {
				Collection<Link> newExtLinks = new HashSet<Link>();

				for (Link link1 : extLinks) {
					LinkedObject bo = link1.getParent();
					OBOProperty r1 = link1.getType();
					for (Link link2 : reasoner.getParents(bo)) {
						LinkedObject co = link2.getParent();
						// not reliable - need to check other paths
						//if (seenObjs.contains(co))
						//	continue;

						OBOProperty r2 = link2.getType();
						Set<OBOProperty> inferredProps = rct.lookup(r1, r2);
						for (OBOProperty inferredProp : inferredProps) {
							Link out = createLink(ao, inferredProp, co);
							if (seenLinks.contains(out))
								continue;
							Link existingLink = reasoner.hasRelationship(ao, inferredProp, co);
							if (existingLink != null) { // we have link already
								// we want to cache at least one explanation, even if it is given
								Collection<Explanation> existingExpls = reasoner.getExplanations(existingLink);
								if (!onlyGiven(existingExpls))
									continue; // we have this already
							}
							
							AbstractExplanation exp;
							exp = new LinkCompositionExplanation(createLink(ao, r1, bo), createLink(bo, r2, co));
							exp.setExplainedLink(out);
							expls.add(exp);
							//logger.debug("HOC: "+out+" // FROM[1]: "+exp);
							if (rct.hasComposition(out.getType()))
								newExtLinks.add(out);
							//seenObjs.add(co);
							seenLinks.add(out);
						}
					}
				}
				extLinks = newExtLinks;

			}
		}


		ruleTime += (System.nanoTime() - time);
		return expls;	
	}



}
