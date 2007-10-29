package org.oboedit.gui.factory;

import java.util.List;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponent;
import org.oboedit.gui.components.TermImageDisplayComponent;

public class TermImageComponentFactory extends AbstractComponentFactory {

	public TermImageComponentFactory() {
		addID("term_image_display");
	}
	
	@Override
	public GUIComponent doCreateComponent(String id) {
		return new TermImageDisplayComponent(id);
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}
	
	public String getName() {
		return "Term Image Display Panel";
	}

}
