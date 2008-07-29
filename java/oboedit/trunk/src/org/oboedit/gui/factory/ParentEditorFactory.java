package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ParentEditor;

import org.apache.log4j.*;

public class ParentEditorFactory extends AbstractComponentFactory<ParentEditor> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ParentEditorFactory.class);

	public ParentEditorFactory() {
	}
	
	public String getID() {
		return "PARENT_EDITOR";
	}

	public ParentEditor doCreateComponent(String id) {
		return new ParentEditor(id);
	}

	public String getName() {
		return "Parent Editor";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.EDITORS;
	}

	@Override
	public String getHelpTopicID() {
		return "Parent_Editor";
	}
}
