package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.OntologyChangeTracker;

import org.apache.log4j.*;

public class OntologyChangeTrackerFactory extends
	AbstractComponentFactory<OntologyChangeTracker> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OntologyChangeTrackerFactory.class);

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
