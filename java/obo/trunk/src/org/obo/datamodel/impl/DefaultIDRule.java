package org.obo.datamodel.impl;

import org.obo.datamodel.*;
import org.obo.filters.*;
import org.obo.identifier.IDRule;

import org.apache.log4j.*;

public class DefaultIDRule implements IDRule {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultIDRule.class);

	protected String rule;
	protected Filter filter;

	public DefaultIDRule() {
		rule = "<your rule>";
		filter = (new CompoundFilterFactory()).createNewFilter();
	}

	public void setRule(String rule) {
		this.rule = rule;
	}

	public void setFilter(Filter filter) {
		this.filter = filter;
	}

	public Filter getFilter() {
		return filter;
	}

	public String getRule() {
		return rule;
	}

	@Override
	public String toString() {
		return getRule();
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof IDRule) || o == null)
			return false;
		IDRule in = (IDRule) o;
		return filter.equals(in.getFilter()) && rule.equals(in.getRule());
	}

	@Override
	public int hashCode() {
		return filter.hashCode() + rule.hashCode();
	}
}
