package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.IntersectionEditor;

import org.apache.log4j.*;

public class IntersectionEditorFactory extends
	AbstractComponentFactory<IntersectionEditor> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IntersectionEditorFactory.class);

	public IntersectionEditorFactory() {
	}
	
	public String getID() {
		return "INTERSECTION_EDITOR";
	}

	public IntersectionEditor doCreateComponent(String id) {
		return new IntersectionEditor(id);
	}

	public String getName() {
		return "Intersection Editor";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.EDITORS;
	}
}
