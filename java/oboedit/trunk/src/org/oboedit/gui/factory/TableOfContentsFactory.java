package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ExtendedInfoComponent;
import org.oboedit.gui.components.TableOfContentsComponent;

public class TableOfContentsFactory extends
		AbstractComponentFactory<TableOfContentsComponent> {

	public TableOfContentsFactory() {
	}
	
	public String getID() {
		return "TABLE_OF_CONTENTS";
	}

	public TableOfContentsComponent doCreateComponent(String id) {
		return new TableOfContentsComponent(id);
	}

	public String getName() {
		return "Table of Contents";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

}
