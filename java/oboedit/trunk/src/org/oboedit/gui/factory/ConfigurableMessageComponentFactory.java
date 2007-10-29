package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponent;
import org.oboedit.gui.components.ConfigurableTextComponent;

public class ConfigurableMessageComponentFactory extends AbstractComponentFactory {

	public ConfigurableMessageComponentFactory() {
		addID("MESSAGE");
	}
	
	@Override
	public GUIComponent doCreateComponent(String id) {
		return new ConfigurableTextComponent(id);
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.MISC;
	}

	public String getName() {
		return "Configurable Message";
	}

}
