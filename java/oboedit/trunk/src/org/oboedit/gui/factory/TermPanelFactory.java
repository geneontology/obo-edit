package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.OBOTermPanel;

import org.apache.log4j.*;

public class TermPanelFactory extends AbstractComponentFactory<OBOTermPanel> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermPanelFactory.class);
	
	public TermPanelFactory() {
	}
	
	public String getID() {
		return "ONTOLOGY_TREE_EDITOR";
	}

	public OBOTermPanel doCreateComponent(String id) {
		return new OBOTermPanel(id);
	}

	public String getName() {
//		return "Ontology Editor Panel";
		return "Ontology Tree Editor";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.ONTOLOGY;
	}
	
	@Override
	public String getHelpTopicID() {
		return "The_Term_Editor_Panel";
	}
}
