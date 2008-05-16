package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.CategoryManagerComponent;

import org.apache.log4j.*;

public class CategoryManagerFactory extends
	AbstractComponentFactory<CategoryManagerComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CategoryManagerFactory.class);

	public CategoryManagerFactory() {
	}
	
	public String getID() {
		return "CATEGORY_MANAGER";
	}

	public CategoryManagerComponent doCreateComponent(String id) {
		return new CategoryManagerComponent(id);
	}

	public String getName() {
		return "Category Manager";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.METADATA;
	}

	@Override
	public String getHelpTopicID() {
		return "Working_with_Categories";
	}
}
