package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ParentEditor;

public class ParentEditorFactory extends AbstractComponentFactory<ParentEditor> {

	public ParentEditorFactory() {
		addID("PARENT_EDITOR");
		addID("plugin:org.geneontology.oboedit.plugin.ParentPlugin");
	}

	public ParentEditor doCreateComponent(String id) {
		return new ParentEditor(id);
	}

	public String getName() {
		return "Parent Editor";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.EDIT;
	}

}
