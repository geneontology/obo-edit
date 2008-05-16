package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.TextEditor;

import org.apache.log4j.*;

public class TextEditorFactory extends AbstractComponentFactory<TextEditor> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextEditorFactory.class);

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

	@Override
	public String getHelpTopicID() {
		return "Text_Editing";
	}
}
