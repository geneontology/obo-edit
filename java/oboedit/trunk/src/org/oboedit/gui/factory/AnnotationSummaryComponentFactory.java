package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.AnnotationSummaryComponent;

public class AnnotationSummaryComponentFactory extends
		AbstractComponentFactory<AnnotationSummaryComponent> {

	public AnnotationSummaryComponentFactory() {
	}

	public AnnotationSummaryComponent doCreateComponent(String id) {
		return new AnnotationSummaryComponent(id);
	}
	
	public String getID() {
		return "ANNOTATION_SUMMARY_COMPONENT";
	}

	public String getName() {
		return "Annotation Summary Component";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

}
