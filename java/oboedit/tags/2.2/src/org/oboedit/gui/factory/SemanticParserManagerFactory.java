package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.SemanticParserManagerComponent;

import org.apache.log4j.*;

public class SemanticParserManagerFactory extends AbstractComponentFactory<SemanticParserManagerComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SemanticParserManagerFactory.class);

	public SemanticParserManagerFactory() {
	}
	
	public String getID() {
		return "SemanticParser_MANAGER";
	}

	public SemanticParserManagerComponent doCreateComponent(String id) {
		return new SemanticParserManagerComponent(id);
	}

	public String getName() {
		return "SemanticParser Manager";
	}
	
	@Override
	public boolean getPreferSeparateWindow() {
		return true;
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.REASONER;
	}

}
