package org.oboedit.gui.components.imageplugin.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.imageplugin.component.TermImageDisplayComponent;

public class TermImageComponentFactory extends
		AbstractComponentFactory<TermImageDisplayComponent> {

	public TermImageComponentFactory() {
	}

	public String getID() {
		return "TERM_IMAGE_DISPLAY";
	}

	@Override
	public TermImageDisplayComponent doCreateComponent(String id) {
		return new TermImageDisplayComponent(id);
	}

	public FactoryCategory getCategory() {
//		return FactoryCategory.INFO;
		return FactoryCategory.VIEWERS;
	}

	public String getName() {
		return "Term Image Display Panel";
	}

	@Override
	public String getHelpTopicID() {
		return "Term_Image_Display_Panel";
	}

}
