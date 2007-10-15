package org.obo.identifier;

import org.obo.filters.*;

public interface IDRule {

	public String getRule();

	public Filter getFilter();

	public void setRule(String rule);

	public void setFilter(Filter filter);
}
