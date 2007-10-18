package org.obo.reasoner.impl;

import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;

public interface ReasonerRule {

	public void install(ReasonedLinkDatabase reasoner);
	public void init(ReasonedLinkDatabase reasoner);
	public Collection<Explanation> getImplications(
			ReasonedLinkDatabase reasoner, Link newLink);
	public void end(ReasonedLinkDatabase reasoner);
	public void uninstall(ReasonedLinkDatabase reasoner);
}
