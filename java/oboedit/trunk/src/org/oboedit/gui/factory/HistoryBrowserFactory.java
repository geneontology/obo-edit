package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.HistoryBrowser;

public class HistoryBrowserFactory extends AbstractComponentFactory<HistoryBrowser> {

	public HistoryBrowserFactory() {
	}
	
	public String getID() {
		return "HISTORY_BROWSER";
	}

	public HistoryBrowser doCreateComponent(String id) {
		return new HistoryBrowser(id);
	}

	public String getName() {
		return "History Browser";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.HISTORY;
	}

}
