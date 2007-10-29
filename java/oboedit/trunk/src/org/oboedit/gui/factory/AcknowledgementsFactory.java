package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponent;
import org.oboedit.gui.components.AcknowledgementsComponent;

public class AcknowledgementsFactory extends AbstractComponentFactory {

	public AcknowledgementsFactory() {
		addID("ACKNOWLEDGEMENTS");
	}
	
	@Override
	public GUIComponent doCreateComponent(String id) {
		return new AcknowledgementsComponent(id);
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.MISC;
	}

	public String getName() {
		return "Acknowledgements";
	}

}
