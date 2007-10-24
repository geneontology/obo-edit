package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.AnnotationSummaryComponent;

public class AnnotationSummaryComponentFactory extends
		AbstractComponentFactory<AnnotationSummaryComponent> {

	public AnnotationSummaryComponentFactory() {
		addID("ANNOTATION_SUMMARY_COMPONENT");
		addID("plugin:org.geneontology.oboedit.plugin.AnnotationSummaryComponentPlugin");
	}

	public AnnotationSummaryComponent doCreateComponent(String id) {
		return new AnnotationSummaryComponent(id);
	}

	public String getName() {
		return "Annotation Summary Component";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

}
