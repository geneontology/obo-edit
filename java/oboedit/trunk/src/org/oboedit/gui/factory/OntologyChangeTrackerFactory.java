package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.OntologyChangeTracker;

public class OntologyChangeTrackerFactory extends
		AbstractComponentFactory<OntologyChangeTracker> {

	public OntologyChangeTrackerFactory() {
	}
	
	public String getID() {
		return "CHANGE_TRACKER";
	}

	public OntologyChangeTracker doCreateComponent(String id) {
		return new OntologyChangeTracker(id);
	}

	public String getName() {
		return "Ontology Change Tracker";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.HISTORY;
	}
}
