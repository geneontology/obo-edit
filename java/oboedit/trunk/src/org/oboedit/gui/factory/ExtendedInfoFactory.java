package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ExtendedInfoComponent;

public class ExtendedInfoFactory extends AbstractComponentFactory<ExtendedInfoComponent> {

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
