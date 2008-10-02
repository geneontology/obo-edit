package org.obo.reasoner.rbr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ExplanationType;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

/**
 * An Explanation for a link inferred via an IntersectionRule
 * 
 * @author cjm
 *
 */
public class IntersectionExplanation extends AbstractExplanation {


	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionExplanation.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 6982440827960096291L;

	protected Collection<IntersectionMatch> matches = new ArrayList<IntersectionMatch>(2);

	public IntersectionExplanation() {
	}
	
	public Collection<IntersectionMatch> getMatches() {
		return matches;
	}

	public void addMatch(IntersectionMatch match) {
		if (!matches.contains(match))
			matches.add(match);
	}

	protected void removeMatch(IntersectionMatch match) {
		matches.remove(match);
	}
	
	public boolean removeEvidence(Link link) {
		for (IntersectionMatch match : matches) {
			if (match.getMatchLink().equals(link)
					|| match.getCompletenessLink().equals(link)) {
				removeMatch(match);
				break;
			}
		}
		boolean broken = !isSupported();
		return broken;
	}
	
	public Collection<Link> getEvidence() {
		ArrayList<Link> out = new ArrayList<Link>(matches.size()*2);
		for(IntersectionMatch m : matches) {
			out.add(m.getCompletenessLink());
			out.add(m.getMatchLink());
		}
		return out;
	}

	protected boolean isSupported() {
		LinkedObject matchParent = getExplainedLink().getParent();
		for(Link link : matchParent.getParents()) {
			if (!TermUtil.isIntersection(link))
				continue;
			boolean found = false;
			for(IntersectionMatch match : matches) {
				if (match.getCompletenessLink().equals(link)) {
					found = true;
				}
			}
			if (!found)
				return false;
		}
		return true;
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.INTERSECTION;
	}

	@Override
	public String toString() {
		return "XP: matches=" + matches;
	}
}
