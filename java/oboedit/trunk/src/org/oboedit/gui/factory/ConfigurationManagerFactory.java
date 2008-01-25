package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ConfigurationManager;

public class ConfigurationManagerFactory extends
		AbstractComponentFactory<ConfigurationManager> {

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
