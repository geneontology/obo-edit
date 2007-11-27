package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.TextEditor;

public class TextEditorFactory extends AbstractComponentFactory<TextEditor> {

	public TextEditorFactory() {
	}
	
	public String getID() {
		return "TEXTEDIT";
	}

	public TextEditor doCreateComponent(String id) {
		TextEditor editor = new TextEditor(id);
		return editor;
	}

	public String getName() {
		return "Text Editor";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.ONTOLOGY;
	}
}
