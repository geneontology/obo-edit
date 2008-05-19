package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.HistoryBrowser;

import org.apache.log4j.*;

public class HistoryBrowserFactory extends AbstractComponentFactory<HistoryBrowser> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HistoryBrowserFactory.class);

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
		return FactoryCategory.TOOLS;
	}

}
