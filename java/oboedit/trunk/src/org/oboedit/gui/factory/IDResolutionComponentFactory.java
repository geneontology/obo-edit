package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.IDResolutionComponent;

public class IDResolutionComponentFactory extends
		AbstractComponentFactory<IDResolutionComponent> {
	
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
		return FactoryCategory.ONTOLOGY;
	}

}
