package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.IDManagerComponent;

public class IDManagerFactory extends AbstractComponentFactory<IDManagerComponent> {

	public IDManagerFactory() {
		addID("ID_MANAGER");
		addID("plugin:org.geneontology.oboedit.plugin.IDManagerPlugin");
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
