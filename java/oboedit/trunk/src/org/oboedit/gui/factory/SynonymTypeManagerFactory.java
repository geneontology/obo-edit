package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.SynonymTypeManager;

import org.apache.log4j.*;

public class SynonymTypeManagerFactory extends
	AbstractComponentFactory<SynonymTypeManager> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymTypeManagerFactory.class);
	public SynonymTypeManagerFactory() {
	}

	public String getID() {
		return "SYNONYM_TYPE_MANAGER";
	}

	public SynonymTypeManager doCreateComponent(String id) {
		return new SynonymTypeManager(id);
	}

	public String getName() {
		return "Synonym Type Manager";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.METADATA;
	}

	@Override
	public String getHelpTopicID() {
		return "Working_with_Synonym_Types";
	}
}
