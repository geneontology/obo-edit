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

	public FactoryCategory getCategory() {
		return FactoryCategory.CONFIG;
	}
}
