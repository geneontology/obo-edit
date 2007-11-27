package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.CrossProductInfoComponent;

public class CrossProductInfoFactory extends
		AbstractComponentFactory<CrossProductInfoComponent> {

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
