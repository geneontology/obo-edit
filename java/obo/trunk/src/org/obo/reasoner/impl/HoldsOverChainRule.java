package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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

public class HoldsOverChainRule extends AbstractReasonerRule {

	protected MultiMap<OBOProperty,OBOProperty> chainMap;

	@Override
	public void init(ReasonedLinkDatabase reasoner) {
		super.init(reasoner);
		buildHoldsOverChainMap(reasoner);
	}

	protected void buildHoldsOverChainMap(ReasonedLinkDatabase reasoner) {
		chainMap = new MultiHashMap<OBOProperty,OBOProperty>();
		for (IdentifiedObject io : reasoner.getObjects()) {
			if (io instanceof OBOProperty) {
				OBOProperty prop = (OBOProperty) io;
				Collection<List<OBOProperty>> chains = prop.getHoldsOverChains();
				if (chains != null) {
					for (List<OBOProperty> chain : chains) {
						if (chain.size() != 2) {
							logger.info("will not do anything with chains of length !=2 : "+chain);
						}
						else {
							for (OBOProperty p2 : chain) {
								chainMap.add(p2, prop);
							}
						}
					}
				}
			}
		}
	}

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HoldsOverChainRule.class);

	public HoldsOverChainRule() {
		setAllowIntersections(true);
	}

	@Override
	protected Collection<Explanation> doGetImplications(
			ReasonedLinkDatabase reasoner, Link newLink) {
		OBOProperty thisRel = newLink.getType();
		Collection<OBOProperty> props = chainMap.get(thisRel);
		if (props == null)
			return null;
		// R holds_over_chain R0, R1, ... Rn
		// we know that newlink has a relation of type oneof R0...Rn
		ArrayList<Explanation> c = new ArrayList<Explanation>();
				
		for (OBOProperty p : props) {
			boolean found = false;
			for (List<OBOProperty> chain : p.getHoldsOverChains()) {
				int pos = chain.indexOf(thisRel);
				if (pos == -1) {
					continue;
				}
				if (pos == 0) {
					// the newlink is the first relation in the chain
					OBOProperty nextRel = chain.get(1);
					for (LinkedObject parent : 
						reasoner.getParentsOfType(newLink.getParent(), nextRel)) {
						Link out = createLink(newLink.getChild(), p, parent);
						AbstractExplanation exp;
						exp = new HoldsOverChainExplanation(newLink);
						exp.setExplainedLink(out);
						c.add(exp);
						logger.debug("HOC: "+out+" // FROM[0]: "+newLink);
					}
					found = true;

				}
				else if (pos == 1) {
					// the newlink is the 2nd relation in the chain
					OBOProperty prevRel = chain.get(0);
					for (Link link : 
						reasoner.getChildren(newLink.getChild())) {
						if (link.getType().equals(prevRel)) {

							Link out = createLink(link.getChild(), p, newLink.getParent());
							AbstractExplanation exp;
							exp = new HoldsOverChainExplanation(newLink);
							exp.setExplainedLink(out);
							c.add(exp);
							logger.debug("HOC: "+out+" // FROM[1]: "+newLink);
						}
					}
					found = true;

				}
				else {
					// uh-oh
					logger.error("assertion error: position of "+thisRel+" in "+p+" is: "+pos);
				}
			}
			if (!found) {
				logger.error("assertion error: could not find "+thisRel);
			}

		}
		return c;
	}

}
