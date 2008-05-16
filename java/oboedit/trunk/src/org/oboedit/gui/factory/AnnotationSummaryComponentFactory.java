package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.AnnotationSummaryComponent;

import org.apache.log4j.*;

public class AnnotationSummaryComponentFactory extends
	AbstractComponentFactory<AnnotationSummaryComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AnnotationSummaryComponentFactory.class);

	public AnnotationSummaryComponentFactory() {
	}

	public AnnotationSummaryComponent doCreateComponent(String id) {
		return new AnnotationSummaryComponent(id);
	}
	
	public String getID() {
		return "ANNOTATION_SUMMARY_COMPONENT";
	}

	public String getName() {
		return "Annotation Summary";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

}
