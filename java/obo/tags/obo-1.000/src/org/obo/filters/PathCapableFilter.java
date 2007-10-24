package org.obo.filters;

import org.obo.datamodel.PathCapable;
import org.obo.reasoner.ReasonedLinkDatabase;

public interface PathCapableFilter<T extends PathCapable> extends Filter<T> {
	public void setReasonedLinkDatabase(ReasonedLinkDatabase reasoner);
}
