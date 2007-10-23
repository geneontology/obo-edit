package org.oboedit.gui;

import org.obo.filters.Filter;

public interface Filterable {


	public void setLinkFilter(Filter<?> filter);
	public void setTermFilter(Filter<?> filter);
	public Filter<?> getTermFilter();
	public Filter<?> getLinkFilter();

}
