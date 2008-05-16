package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.CrossProductInfoComponent;

import org.apache.log4j.*;

public class CrossProductInfoFactory extends
	AbstractComponentFactory<CrossProductInfoComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CrossProductInfoFactory.class);

	public CrossProductInfoFactory() {
	}
	
	public String getID() {
		return "CROSS_PRODUCT_INFO_COMPONENT";
	}

	public CrossProductInfoComponent doCreateComponent(String id) {
		return new CrossProductInfoComponent(id);
	}

	public String getName() {
		return "Cross Product Info";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

}
