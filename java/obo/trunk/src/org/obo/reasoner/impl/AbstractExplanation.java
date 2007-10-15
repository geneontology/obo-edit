package org.obo.reasoner.impl;

import java.util.*;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;
import org.obo.reasoner.Explanation;

public abstract class AbstractExplanation implements Explanation {

	protected Collection<Link> supportingLinks = new LinkedList<Link>();

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
		if (!supportingLinks.contains(link))
			supportingLinks.add(link);
	}

	public boolean removeEvidence(Link link) {
		return supportingLinks.remove(link);
	}

	public int hashCode() {
		return getExplanationType().hashCode() + getEvidence().hashCode();
	}

	public void setDesc(String desc) {
	}
}
