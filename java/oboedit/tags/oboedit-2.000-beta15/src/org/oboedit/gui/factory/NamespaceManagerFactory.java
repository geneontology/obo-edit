package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.NamespaceManager;

public class NamespaceManagerFactory extends AbstractComponentFactory<NamespaceManager> {

	public NamespaceManagerFactory() {
		addID("NAMESPACE_MANAGER");
		addID("plugin:org.geneontology.oboedit.plugin.NamespaceManagerPlugin");
	}
	
	public NamespaceManager doCreateComponent(String id) {
		return new NamespaceManager(id);
	}

	public String getName() {
		return "Namespace Manager";
	}
		
	
	public FactoryCategory getCategory() {
		return FactoryCategory.METADATA;
	}
}
