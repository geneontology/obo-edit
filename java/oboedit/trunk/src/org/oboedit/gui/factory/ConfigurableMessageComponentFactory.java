package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponent;
import org.oboedit.gui.components.ConfigurableTextComponent;

public class ConfigurableMessageComponentFactory extends AbstractComponentFactory {

	public ConfigurableMessageComponentFactory() {
	}
	
	public String getID() {
		return "MESSAGE";
	}
	
	@Override
	public GUIComponent doCreateComponent(String id) {
		return new ConfigurableTextComponent(id);
	}

	public FactoryCategory getCategory() {
//		return FactoryCategory.MISC;
		return FactoryCategory.CONFIG;
	}

	public String getName() {
		return "Configurable Message";
	}

	@Override
	public String getHelpTopicID() {
		return "Configurable_Message";
	}

}
