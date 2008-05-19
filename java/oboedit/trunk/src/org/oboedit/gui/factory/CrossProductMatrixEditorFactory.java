package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.CrossProductMatrixEditorComponent;

import org.apache.log4j.*;

public class CrossProductMatrixEditorFactory extends
	AbstractComponentFactory<CrossProductMatrixEditorComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CrossProductMatrixEditorFactory.class);

	public CrossProductMatrixEditorFactory() {
	}
	
	public String getID() {
		return "CROSS_PRODUCT_MATRIX_EDITOR_COMPONENT";
	}

	public CrossProductMatrixEditorComponent doCreateComponent(String id) {
		return new CrossProductMatrixEditorComponent(id);
	}

	public String getName() {
		return "Cross-Product Matrix Editor";
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.EDITORS;
	}

}
