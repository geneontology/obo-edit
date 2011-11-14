package org.oboedit.gui;

import org.obo.filters.Filter;

public interface Filterable extends ObjectSelector {


	public void setLinkFilter(Filter<?> filter);
	public void setTermFilter(Filter<?> filter);
	public Filter<?> getTermFilter();
	public Filter<?> getLinkFilter();

}
