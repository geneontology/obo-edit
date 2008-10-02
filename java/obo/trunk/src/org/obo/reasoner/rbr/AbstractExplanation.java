package org.obo.reasoner.rbr;

import java.util.*;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;
import org.obo.reasoner.Explanation;

public abstract class AbstractExplanation implements Explanation {

	protected Collection<Link> supportingLinks;

	/**
	 * Each explanation explains a single link which has been inferred by a particular rule
	 */
	protected Link explainedLink;

	public PathCapable getExplainedObject() {
		return getExplainedLink();
	}

	public Link getExplainedLink() {
		return explainedLink;
	}

	public void setExplainedLink(Link explainedLink) {
		this.explainedLink = explainedLink;
	}

	public Collection<Link> getEvidence() {
		if (supportingLinks == null)
			return Collections.emptySet();
		else
			return supportingLinks;
	}

	public String getDesc() {
		return null;
	}

	public boolean equals(Object o) {
		if (o instanceof AbstractExplanation) {
			AbstractExplanation exp = (AbstractExplanation) o;
			return exp.getExplanationType().equals(getExplanationType())
					&& exp.getEvidence().equals(getEvidence());
		} else
			return false;
	}

	public void addEvidence(Link link) {
		if (supportingLinks == null)
			 supportingLinks = new ArrayList<Link>(1);
		if (!supportingLinks.contains(link))
			supportingLinks.add(link);
	}

	public boolean removeEvidence(Link link) {
		boolean collapsed = false;
		if (supportingLinks != null)
			collapsed = supportingLinks.remove(link);
		if (supportingLinks.isEmpty()) {
			supportingLinks = null;
			collapsed = true;
		}
		return collapsed;
	}
	
	public String toString() {
		return getExplanationType() + " explained:" + explainedLink + " supported_by_n: " + getEvidence().size();
	}

	public int hashCode() {
		return getExplanationType().hashCode() + getEvidence().hashCode();
	}

	public void setDesc(String desc) {
	}
}
