package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.SearchResultsComponent;

public class SearchResultsComponentFactory extends
		AbstractComponentFactory<SearchResultsComponent> {

	public SearchResultsComponentFactory() {
	}
	
	public String getID() {
		return "SEARCH_RESULTS";
	}

	@Override
	public SearchResultsComponent doCreateComponent(String id) {
		return new SearchResultsComponent(id);
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

	@Override
	public boolean showInMenus() {
		return false;
	}

	public String getName() {
		return "Search Results";
	}

}
