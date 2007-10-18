package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ExtendedInfoComponent;

public class ExtendedInfoFactory extends AbstractComponentFactory<ExtendedInfoComponent> {

	public ExtendedInfoFactory() {
		addID("EXTENDED_INFO");
		addID("plugin:org.geneontology.oboedit.plugin.ExtendedInfoPlugin");
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
