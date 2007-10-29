package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.CrossProductMatrixEditorComponent;

public class CrossProductMatrixEditorFactory extends
		AbstractComponentFactory<CrossProductMatrixEditorComponent> {

	public CrossProductMatrixEditorFactory() {
		addID("CROSS_PRODUCT_MATRIX_EDITOR_COMPONENT");
		addID("plugin:org.geneontology.oboedit.plugin.CrossProductMatrixEditorPlugin");
	}

	public CrossProductMatrixEditorComponent doCreateComponent(String id) {
		return new CrossProductMatrixEditorComponent(id);
	}

	public String getName() {
		return "Cross-Product Matrix Editor";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.ONTOLOGY;
	}

}
