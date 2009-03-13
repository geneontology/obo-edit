package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.IDResolutionComponent;

import org.apache.log4j.*;

public class IDResolutionComponentFactory extends
	AbstractComponentFactory<IDResolutionComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDResolutionComponentFactory.class);
	
	public IDResolutionComponentFactory() {
	}
	
	public String getID() {
		return "ID_RESOLUTION";
	}

	public IDResolutionComponent doCreateComponent(String id) {
		return new IDResolutionComponent(id);
	}

	public String getName() {
		return "ID Fixer";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.TOOLS;
	}

}
