package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.components.SearchComponent;

import org.apache.log4j.*;

public class SearchComponentFactory extends AbstractComponentFactory<SearchComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SearchComponentFactory.class);

	public SearchComponentFactory() {
	}
	
	public String getID() {
		return "FIND";
	}

	public SearchComponent doCreateComponent(String id) {
		return new SearchComponent(id, new TermFilterEditorFactory());
	}

	public String getName() {
		return "Search Panel";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.SEARCH;
	}

	@Override
	public String getHelpTopicID() {
		return "Searching";
	}
}
