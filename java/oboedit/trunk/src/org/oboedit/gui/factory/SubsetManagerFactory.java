package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.SubsetManagerComponent;

import org.apache.log4j.*;

public class SubsetManagerFactory extends
	AbstractComponentFactory<SubsetManagerComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SubsetManagerFactory.class);

	public SubsetManagerFactory() {
	}
	
	public String getID() {
		return "SUBSET_MANAGER";
	}

	public SubsetManagerComponent doCreateComponent(String id) {
		return new SubsetManagerComponent(id);
	}

	public String getName() {
		return "Subset Manager";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.METADATA;
	}

	@Override
	public String getHelpTopicID() {
		return "Working_with_Subsets";
	}
}
