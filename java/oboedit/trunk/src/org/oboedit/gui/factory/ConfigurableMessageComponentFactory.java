package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponent;
import org.oboedit.gui.components.ConfigurableTextComponent;

import org.apache.log4j.*;

public class ConfigurableMessageComponentFactory extends AbstractComponentFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ConfigurableMessageComponentFactory.class);

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
