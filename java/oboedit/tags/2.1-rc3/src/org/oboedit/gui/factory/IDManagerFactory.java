package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.IDManagerComponent;

import org.apache.log4j.*;

public class IDManagerFactory extends AbstractComponentFactory<IDManagerComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDManagerFactory.class);

	public IDManagerFactory() {
	}
	
	public String getID() {
		return "ID_MANAGER";
	}
	
	public IDManagerComponent doCreateComponent(String id) {
		return new IDManagerComponent(id);
	}

	public String getName() {
		return "ID Manager";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.METADATA;
	}

	@Override
	public String getHelpTopicID() {
		return "The_ID_Manager_Plugin";
	}
}
