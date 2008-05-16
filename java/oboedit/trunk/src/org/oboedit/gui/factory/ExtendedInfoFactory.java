package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ExtendedInfoComponent;

import org.apache.log4j.*;

public class ExtendedInfoFactory extends AbstractComponentFactory<ExtendedInfoComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExtendedInfoFactory.class);

	public ExtendedInfoFactory() {
	}
	
	public String getID() {
		return "EXTENDED_INFO";
	}

	public ExtendedInfoComponent doCreateComponent(String id) {
		return new ExtendedInfoComponent(id);
	}

	public String getName() {
		return "Extended Info";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}


}
