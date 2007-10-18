package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.SearchComponent;

public class SearchComponentFactory extends AbstractComponentFactory<SearchComponent> {

	public SearchComponentFactory() {
		addID("FIND");
	}

	public SearchComponent doCreateComponent(String id) {
		return new SearchComponent(id);
	}

	public String getName() {
		return "Search Panel";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.SEARCH;
	}
}
