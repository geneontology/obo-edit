package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.components.SearchComponent;

import org.apache.log4j.*;

public class LinkSearchComponentFactory extends AbstractComponentFactory<SearchComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkSearchComponentFactory.class);

	public LinkSearchComponentFactory() {
	}
	
	public String getID() {
		return "FIND_LINKS";
	}

	public SearchComponent doCreateComponent(String id) {
		return new SearchComponent(id, new LinkFilterEditorFactory());
	}

	public String getName() {
		return "Link Search Panel";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.SEARCH;
	}
}
