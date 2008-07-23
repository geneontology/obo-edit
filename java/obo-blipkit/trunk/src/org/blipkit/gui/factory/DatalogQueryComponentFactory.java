package org.blipkit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.blipkit.gui.components.DatalogQueryComponent;

public class DatalogQueryComponentFactory extends
		AbstractComponentFactory<DatalogQueryComponent> {

	public DatalogQueryComponentFactory() {
//		addID("DATALOG_COMPONENT");
//		addID("plugin:org.blipkit.plugin.DatalogQueryPlugin");
	}

	public DatalogQueryComponent doCreateComponent(String id) {
		return new DatalogQueryComponent(id);
	}

	public String getName() {
		return "Datalog Query Component";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.ONTOLOGY;
	}

	public String getID() {
		return "plugin:org.blipkit.plugin.DatalogQueryPlugin";
	}

}
