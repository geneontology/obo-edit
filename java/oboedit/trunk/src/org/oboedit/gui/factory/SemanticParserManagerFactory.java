package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.SemanticParserManagerComponent;

public class SemanticParserManagerFactory extends AbstractComponentFactory<SemanticParserManagerComponent> {

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
