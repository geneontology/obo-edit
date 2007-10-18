package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ReasonerManagerComponent;

public class ReasonerManagerFactory extends AbstractComponentFactory<ReasonerManagerComponent> {

	public ReasonerManagerFactory() {
		addID("REASONER_MANAGER");
		addID("plugin:org.geneontology.oboedit.plugin.ReasonerPlugin");
	}

	public ReasonerManagerComponent doCreateComponent(String id) {
		return new ReasonerManagerComponent(id);
	}

	public String getName() {
		return "Reasoner Manager";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.REASONER;
	}

}
