package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.components.SearchComponent;

public class LinkSearchComponentFactory extends AbstractComponentFactory<SearchComponent> {

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
