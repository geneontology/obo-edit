package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.CategoryManagerComponent;

public class CategoryManagerFactory extends
		AbstractComponentFactory<CategoryManagerComponent> {

	public CategoryManagerFactory() {
		addID("CATEGORY_MANAGER");
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
}
