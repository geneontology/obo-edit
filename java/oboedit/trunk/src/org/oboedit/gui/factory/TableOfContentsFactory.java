package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ExtendedInfoComponent;
import org.oboedit.gui.components.TableOfContentsComponent;

import org.apache.log4j.*;

public class TableOfContentsFactory extends
	AbstractComponentFactory<TableOfContentsComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TableOfContentsFactory.class);

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

	@Override
	public String getHelpTopicID() {
		return "Table_of_Contents";
	}
}
