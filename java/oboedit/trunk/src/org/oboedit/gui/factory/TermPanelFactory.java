package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.OBOTermPanel;

public class TermPanelFactory extends AbstractComponentFactory<OBOTermPanel> {
	
	public TermPanelFactory() {
	}
	
	public String getID() {
		return "DAG";
	}

	public OBOTermPanel doCreateComponent(String id) {
		return new OBOTermPanel(id);
	}

	public String getName() {
		return "Ontology Editor Panel";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.ONTOLOGY;
	}
}
