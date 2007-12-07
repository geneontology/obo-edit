package org.oboeditplugins.imageplugin.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboeditplugins.imageplugin.component.TermImageDisplayComponent;

public class TermImageComponentFactory extends
		AbstractComponentFactory<TermImageDisplayComponent> {

	public TermImageComponentFactory() {
	}

	public String getID() {
		return "term_image_display";
	}

	@Override
	public TermImageDisplayComponent doCreateComponent(String id) {
		return new TermImageDisplayComponent(id);
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

	public String getName() {
		return "Term Image Display Panel";
	}

}
