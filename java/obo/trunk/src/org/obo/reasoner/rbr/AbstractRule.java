package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.log4j.Logger;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ExplanationType;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.TermUtil;

public abstract class AbstractRule implements Rule {

	public long ruleTime;
	protected boolean allowIntersections = false;
	//initialize logger
	protected final static Logger logger = Logger.getLogger(AbstractRule.class);

	public void setAllowIntersections(boolean allowIntersections) {
		this.allowIntersections = allowIntersections;
	}

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

	public Collection<Explanation> getNewInferencesForComposition(ReasonedLinkDatabase reasoner, OBOProperty inferredProp, OBOProperty p0, OBOProperty p1) {
		long time;
		time = System.nanoTime();
		ArrayList<Explanation> expls = new ArrayList<Explanation>();
		Collection<Link> links0 = reasoner.getLinks(p0);
		for (Link link0 : links0) {
			for (LinkedObject link1parent : reasoner.getParentsOfType(link0.getParent(), p1)) {
				Link existingLink = reasoner.hasRelationship(link0.getChild(), inferredProp, link1parent);
				if (existingLink != null) {
					Collection<Explanation> existingExpls = reasoner.getExplanations(existingLink);
					if (!onlyGiven(existingExpls))
						continue; // we have this already
				}
				Link out = createLink(link0.getChild(), inferredProp, link1parent);
				AbstractExplanation exp;
				exp = new LinkCompositionExplanation(link0, createLink(link0.getParent(), p1, link1parent));
				exp.setExplainedLink(out);
				expls.add(exp);
				//logger.debug("HOC: "+out+" // FROM[1]: "+exp);
			}		
		}
		ruleTime += (System.nanoTime() - time);
		return expls;	
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

}
