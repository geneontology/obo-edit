package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.HistoryBrowser;

public class HistoryBrowserFactory extends AbstractComponentFactory<HistoryBrowser> {

	public HistoryBrowserFactory() {
		addID("HISTORY_BROWSER");
		addID("plugin:org.geneontology.oboedit.plugin.HistoryPlugin");
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
