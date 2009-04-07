package org.obo.filters;

import java.util.Collection;

import org.apache.log4j.Logger;
import org.obo.datamodel.Link;

public abstract class AbstractBooleanLinkCriterion extends
		AbstractCriterion<Link, Boolean> implements BooleanCriterion<Link> {

//	initialize logger
	protected final static Logger logger = Logger.getLogger(AbstractBooleanLinkCriterion.class);
	
	public Class<Boolean> getReturnType() {
		return Boolean.class;
	}

	public Class<Link> getInputType() {
		return Link.class;
	}

	public Collection<Boolean> getValues(Collection<Boolean> scratch, Link obj) {
		logger.debug("AbstractBooleanLinkCriterion.getValues");
		if (matches(obj))
			scratch.add(Boolean.TRUE);
		else
			scratch.add(Boolean.FALSE);

		return scratch;
	}

	public boolean isLegal(String value) {
		return true;
	}
}
