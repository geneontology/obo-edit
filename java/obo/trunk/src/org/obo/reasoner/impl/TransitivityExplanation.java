package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.reasoner.ExplanationType;

public class TransitivityExplanation extends AbstractExplanation {
	/**
	 * 
	 */
	private static final long serialVersionUID = -4952593548062292144L;
	protected Link directLink;
	protected Link extensionLink;
	
	public TransitivityExplanation(Link explainedLink, Link directLink, Link extensionLink) {
		this(directLink, extensionLink);
		setExplainedLink(explainedLink);
	}
	
	public TransitivityExplanation() {}

	public TransitivityExplanation(Link directLink, Link extensionLink) {
		if (!directLink.getParent().equals(extensionLink.getChild()))
			throw new IllegalArgumentException();
		this.directLink = directLink;
		this.extensionLink = extensionLink;
	}
	
	public Link getDirectLink() {
		return directLink;
	}
	
	public Link getExtensionLink() {
		return extensionLink;
	}
	
	@Override
	public Collection<Link> getEvidence() {
		ArrayList<Link> out = new ArrayList<Link>(2);
		out.add(directLink);
		out.add(extensionLink);
		return out;
	}
	
	@Override
	public void addEvidence(Link link) {
		// TODO Auto-generated method stub
		super.addEvidence(link);
	}
	
	@Override
	public boolean removeEvidence(Link link) {
		if (directLink.equals(link)) {
			directLink = null;
			return true;
		} else if (extensionLink.equals(link)) {
			extensionLink = null;
			return true;
		} else
			return false;
	}

	public ExplanationType getExplanationType() {
		return ExplanationType.TRANSITIVITY;
	}

	@Override
	public String toString() {
		return "TRANSITIVITY: "+explainedLink+" from " + extensionLink + " over "
				+ directLink;
	}
}
