package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ExplanationComponent;

public class ExplanationComponentFactory extends
		AbstractComponentFactory<ExplanationComponent> {
	public ExplanationComponentFactory() {
	}
	
	public String getID() {
		return "EXPLANATIONS";
	}

	public ExplanationComponent doCreateComponent(String id) {
		return new ExplanationComponent(id);
	}

	public String getName() {
		return "Explanations";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.REASONER;
	}
}
