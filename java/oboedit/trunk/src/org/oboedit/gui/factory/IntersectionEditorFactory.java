package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.IntersectionEditor;

public class IntersectionEditorFactory extends
		AbstractComponentFactory<IntersectionEditor> {

	public IntersectionEditorFactory() {
		addID("INTERSECTION_EDITOR");
	}

	public IntersectionEditor doCreateComponent(String id) {
		return new IntersectionEditor(id);
	}

	public String getName() {
		return "Intersection Editor";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.ONTOLOGY;
	}
}
