package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.GlobalFilterManagerComponent;

import org.apache.log4j.*;

public class GlobalFilterManagerFactory extends
	AbstractComponentFactory<GlobalFilterManagerComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GlobalFilterManagerFactory.class);

	public GlobalFilterManagerFactory() {
	}
	
	public String getID() {
		return "GLOBAL_FILTER_MANAGER";
	}

	public GlobalFilterManagerComponent doCreateComponent(String id) {
		return new GlobalFilterManagerComponent(id);
	}

	public String getName() {
		return "Global Filters";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.CONFIG;
	}

	@Override
	public String getHelpTopicID() {
	    return "Global_Filters___Renderers";
	}

}
