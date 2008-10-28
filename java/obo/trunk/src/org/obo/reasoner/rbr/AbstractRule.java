package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

public abstract class AbstractRule implements Rule {

	public long ruleTime;
	//initialize logger
	protected final static Logger logger = Logger.getLogger(AbstractRule.class);


	public void end(ReasonedLinkDatabase reasoner) {

	}

	public void init(ReasonedLinkDatabase reasoner) {
		ruleTime = 0;
	}

	public void install(ReasonedLinkDatabase reasoner) {

	}

	public void uninstall(ReasonedLinkDatabase reasoner) {

	}

	protected Link createLink(LinkedObject child, OBOProperty type,
			LinkedObject parent) {
		return new RuleBasedReasoner.ReasonerLink(child, type, parent);
	}


	public boolean isRedundant(ReasonedLinkDatabase reasoner, Link link) {
		Collection<Explanation> existingExpls = reasoner.getExplanations(link);
		//logger.debug("  existing expls for "+link+" ==::== "+existingExpls);
		if (existingExpls.size() == 0)
			return false;
		if (existingExpls.size() == 1) {
			if (existingExpls.iterator().next().getExplanationType().equals(ExplanationType.GIVEN)) {
				return false;
			}

		}	
		return true; // the reasoner has given us this already
	}

	protected boolean onlyGiven(Collection<Explanation> expls) {
		return 
		expls.size() == 1 && expls.iterator().next().getExplanationType().equals(ExplanationType.GIVEN);
	}

	// TODO - make this more efficient. We spend a lot of time recalculating the same link
	public Collection<Explanation> getNewInferencesForComposition(ReasonedLinkDatabase reasoner, OBOProperty inferredProp, OBOProperty p0, OBOProperty p1) {
		long time;
		time = System.nanoTime();

		// a p0 b, b p1 c -> a ip c
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		for (IdentifiedObject a : reasoner.getObjects()) {
			if (!(a instanceof LinkedObject)) {
				continue;
			}
			LinkedObject ao = (LinkedObject) a;

			Collection<LinkedObject> seenObjs = new HashSet<LinkedObject>();

			Collection<LinkedObject> extObjs = new HashSet<LinkedObject>();
			extObjs.addAll(reasoner.getParentsOfType(ao, p0));
			while (extObjs.size() > 0) {
				Collection<LinkedObject> newExtObjs = new HashSet<LinkedObject>();

				//for (LinkedObject bo : reasoner.getParentsOfType(ao, p0)) {
				for (LinkedObject bo : extObjs) {
					for (LinkedObject co : reasoner.getParentsOfType(bo, p1)) {
						if (seenObjs.contains(co))
							continue;
						Link existingLink = reasoner.hasRelationship(ao, inferredProp, co);
						if (existingLink != null) { // we have link already
							// we want to cache at least one explanation, even if it is given
							Collection<Explanation> existingExpls = reasoner.getExplanations(existingLink);
							if (!onlyGiven(existingExpls))
								continue; // we have this already
						}
						Link out = createLink(ao, inferredProp, co);
						AbstractExplanation exp;
						exp = new LinkCompositionExplanation(createLink(ao, p0, bo), createLink(bo, p1, co));
						exp.setExplainedLink(out);
						expls.add(exp);
						//logger.debug("HOC: "+out+" // FROM[1]: "+exp);
						if (inferredProp.equals(p0)) {
							newExtObjs.add(co);
						}
						seenObjs.add(co);
					}
				}
				extObjs = newExtObjs;

			}
		}


		ruleTime += (System.nanoTime() - time);
		return expls;	
	}


}
