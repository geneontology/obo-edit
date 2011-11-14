package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.NamespaceManager;

import org.apache.log4j.*;

public class NamespaceManagerFactory extends AbstractComponentFactory<NamespaceManager> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NamespaceManagerFactory.class);

	public NamespaceManagerFactory() {
	}
	
	public String getID() {
		return "NAMESPACE_MANAGER";
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

	@Override
	public String getHelpTopicID() {
		return "Working_with_Namespaces";
	}
}
