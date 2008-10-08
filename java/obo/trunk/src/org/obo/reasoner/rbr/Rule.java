package org.obo.reasoner.rbr;

import java.util.Collection;

import org.obo.datamodel.Link;
import org.obo.reasoner.Explanation;
import org.obo.reasoner.ReasonedLinkDatabase;

public interface Rule {

	public void install(ReasonedLinkDatabase reasoner);
	public void init(ReasonedLinkDatabase reasoner);
	public Collection<Explanation> getNewInferences(ReasonedLinkDatabase reasoner);
	public void end(ReasonedLinkDatabase reasoner);
	public void uninstall(ReasonedLinkDatabase reasoner);
	public boolean isRedundant(ReasonedLinkDatabase reasoner, Link link);
}
