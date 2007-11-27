package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.IDManagerComponent;

public class IDManagerFactory extends AbstractComponentFactory<IDManagerComponent> {

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
}
