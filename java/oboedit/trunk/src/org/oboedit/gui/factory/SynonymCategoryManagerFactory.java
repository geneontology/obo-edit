package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.SynonymCategoryManager;

import org.apache.log4j.*;

public class SynonymCategoryManagerFactory extends
	AbstractComponentFactory<SynonymCategoryManager> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymCategoryManagerFactory.class);
	public SynonymCategoryManagerFactory() {
	}

	public String getID() {
		return "SYNONYM_CATEGORY_MANAGER";
	}

	public SynonymCategoryManager doCreateComponent(String id) {
		return new SynonymCategoryManager(id);
	}

	public String getName() {
		return "Synonym Category Manager";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.METADATA;
	}

	@Override
	public String getHelpTopicID() {
		return "Working_with_Synonym_Categories";
	}
}
