package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ConfigurationManager;

import org.apache.log4j.*;

public class ConfigurationManagerFactory extends
	AbstractComponentFactory<ConfigurationManager> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ConfigurationManagerFactory.class);

	public ConfigurationManagerFactory() {
	}

	public ConfigurationManager doCreateComponent(String id) {
		return new ConfigurationManager(id);
	}
	
	public String getID() {
		return "CONFIGURATION_MANAGER";
	}

	public String getName() {
		return "Configuration Manager";
	}
	
	@Override
	public boolean getPreferSeparateWindow() {
		return true;
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.CONFIG;
	}

	@Override
	public String getHelpTopicID() {
		return "Configuration_Manager";
	}
}
