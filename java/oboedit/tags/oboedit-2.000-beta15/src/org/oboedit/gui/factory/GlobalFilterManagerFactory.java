package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.GlobalFilterManagerComponent;

public class GlobalFilterManagerFactory extends
		AbstractComponentFactory<GlobalFilterManagerComponent> {

	public GlobalFilterManagerFactory() {
		addID("GLOBAL_FILTER_MANAGER");
		addID("plugin:org.geneontology.oboedit.plugin.GlobalFilterPlugin");
	}

	public GlobalFilterManagerComponent doCreateComponent(String id) {
		return new GlobalFilterManagerComponent(id);
	}

	public String getName() {
		return "Global Filters";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.CONFIG;
	}

}
