package org.oboedit.gui;

import org.obo.filters.*;

public interface Filterable {

	public void setFilter(FilterPair filter);

	public FilterPair getFilter();

}
